package org.aphreet.c3

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import loc.GroupWikiLoc
import org.aphreet.c3.model._
import net.liftweb.mapper._
import net.liftweb.http._

import js.jquery.JQuery14Artifacts

import net.liftweb.widgets.logchanger._
import net.liftweb.widgets.uploadprogress._

import org.aphreet.c3.logging.LogLevel
import net.liftweb.widgets.tablesorter.TableSorter
import net.liftweb.widgets.autocomplete.AutoComplete
import net.liftweb.widgets.flot.Flot

import net.liftweb.widgets.menu.MenuWidget
import org.aphreet.c3.helpers.C3Streamer


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Bootable{

  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    LiftRules.resourceNames = "i18n/lift-core" :: LiftRules.resourceNames

    LiftRules.jsArtifacts = JQuery14Artifacts

    // where to search snippet
    LiftRules.addToPackages("org.aphreet.c3")
    Schemifier.schemify(true, Schemifier.infoF _, User, Group, Category, Tag, UserGroup)

    val isIE = Test( req => req.isIE )

    lazy val loginUrl = "/user_mgt/login"

    // stateful redirect after login
    def loginAndComeBack = {
      val uri = S.uriAndQueryString
      RedirectWithState ( loginUrl, RedirectState( () => User.loginRedirect.set(uri) , "Not logged in" -> NoticeType.Notice ) )
    }

    val loggedIn = If(() => User.loggedIn_?, loginAndComeBack _ )

    val isSuperAdmin = If(() => {if(!User.currentUser.isEmpty) User.currentUser.open_!.superUser.is else false},
      () => RedirectWithState("/index", RedirectState( () => {} ,"Not a super user" -> NoticeType.Notice ) )
    )

    val isGroupAdmin = If(() => {
      (for {
        groupName <- S.param("groupname")
        group     <- Group.find(By(Group.name,groupName))
        user      <- User.currentUser
        if user.id.is == group.owner.is
      } yield true).openOr(false)
    }, () => RedirectWithState("/index", RedirectState( () => {} ,"Not a group admin" -> NoticeType.Notice )))

    // Build SiteMap
    def sitemap() = SiteMap(

      Menu("Home") / "index" >> LocGroup("mainmenu"),

      Menu("IEDisclaimer") / "ie_disclaimer" >> isIE >> Hidden,

      Menu("About") / "about" >> LocGroup("footerMenu"),

      Menu("Faq") / "faq" >> LocGroup("footerMenu"),

      Menu("Users") / "users" / "index" >> isSuperAdmin >> LocGroup("mainmenu"),

      Menu("Categories") / "categories" >> loggedIn >> LocGroup("mainmenu"),

      Menu("Groups") / "groups" >> loggedIn >> LocGroup("mainmenu"),

      Menu("GroupOverview") / "groupsection" / "index" >> loggedIn >> Hidden,

      Menu("GroupFiles") / "groupsection" / "files" >> loggedIn >> Hidden,

      Menu("GroupFiles") / "groupsection" / "file" >> loggedIn >> Hidden,

      Menu("GroupWiki") / "groupsection" / "wiki-view" >> loggedIn >> Hidden,

      Menu("GroupWiki") / "groupsection" / "wiki-edit" >> loggedIn >> Hidden,

      Menu("GroupAdmin") / "groupsection" / "admin" >> loggedIn >> Hidden >> isGroupAdmin,

      Menu("UserEdit") / "users" / "edituser" >> loggedIn >> Hidden,

      Menu("Search") / "search" >> loggedIn >> Hidden,

      Menu(GroupWikiLoc),
    
      LogLevel.menu // default log level menu is located at /loglevel/change
    )

    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))

    // Custom 404 page
    LiftRules.uriNotFound.prepend(NamedPF("404handler"){
      case (req,failure) =>
        NotFoundAsTemplate(ParsePath(List("404"),"html", false, false))
    })

    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupFilesRewrite") {
      case RewriteRequest(
      ParsePath("group" :: groupname  :: "files" :: directory , extension, _,_), _, _) => {

        val dotExt = extension match {
          case "" => ""
          case str => "."+str
        }
        Group.find(By(Group.name,groupname)) match {
          case Full(group) => {
            // TODO if resource instance is File ==>> rewrite request to file page
            C3Resource.get(group,directory.mkString("/") + dotExt) match {
              case Some(resource) if(resource.isInstanceOf[File]) => {
                RewriteResponse("groupsection" :: "file" :: Nil, Map("groupname" -> groupname,"groupdirectory" -> (directory.mkString("/") + dotExt),"filepath" -> (directory.mkString("/") + dotExt), "rewrite" -> "groupFiles"))
              }
              case Some(resource) =>
                RewriteResponse("groupsection" :: "files" :: Nil, Map("groupname" -> groupname,"groupdirectory" -> directory.mkString("/"), "rewrite" -> "groupFiles"))
              case _ =>
                RewriteResponse("404" :: Nil)
            }
          }
          case _ => RewriteResponse("404" :: Nil)
        }
      }
    })

    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupOverviewRewrite") {
      case RewriteRequest(
      ParsePath("group" :: groupname  :: Nil , _, _,_), _, _) =>
        RewriteResponse(
          "groupsection" ::  "index" :: Nil, Map("groupname" -> groupname, "rewrite" -> "groupOverview")
        )
    })
    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupWikiRewrite") {
      case RewriteRequest(
      ParsePath("group" :: groupname  :: "wiki" :: pagename :: Nil , _, _,_), _, _) =>
        RewriteResponse(
          "groupsection" ::  "wiki-view" :: Nil, Map("groupname" -> groupname, "pagename" -> pagename,"rewrite" -> "groupWiki")
        )
    })

    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupRewriteWikiMain") {
      case RewriteRequest(
      ParsePath("group" :: groupname  :: "wiki" :: Nil , _, _,_), _, _) =>
        RewriteResponse(
          "groupsection" ::  "wiki-view" :: Nil, Map("groupname" -> groupname, "pagename" -> "Main", "rewrite" -> "groupWikiMain")
        )
    })

    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupAdminRewrite") {
      case RewriteRequest(
      ParsePath("group" :: groupname  :: "admin" :: Nil , _, _,_), _, _) =>
        RewriteResponse(
          "groupsection" ::  "admin" :: Nil, Map("groupname" -> groupname)
        )
    })

    LiftRules.dispatch.append {
      case Req("download" :: groupname :: filePath, extension, GetRequest) =>
        C3Streamer(groupname, filePath, extension)
    }

    // for ajax file upload
    import org.aphreet.c3.lib.FileUpload
    LiftRules.dispatch.append(FileUpload)

    LiftRules.statelessRewrite.prepend(NamedPF("ParticularUserRewrite") {
      case RewriteRequest(
      ParsePath("user" :: useremail  :: Nil , _, _,_), _, _) =>
        RewriteResponse(
          "users" ::  "edituser" :: Nil, Map("useremail" -> useremail)
        )
    })

    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupOverviewRewriteWikiEdit") {
      case RewriteRequest(
      ParsePath("group" :: groupname  :: "wiki" :: pagename :: "edit" :: Nil , _, _,_), _, _) =>
        RewriteResponse(
          "groupsection" ::  "wiki-edit" :: Nil, Map("groupname" -> groupname, "pagename" -> pagename)
        )
    })


    LiftRules.statelessRewrite.prepend(NamedPF("IENotSupportedDisclaimerLoginRewrite") {
      case RewriteRequest(
      ParsePath("user_mgt" :: _ :: Nil , _, _,_), _, req)  if( req.userAgent.map(_.contains("MSIE")) openOr(false) ) =>
        RewriteResponse(
          "ie_disclaimer" :: Nil
        )
    })

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // 1 hour timeout for long requests
    LiftRules.ajaxPostTimeout = 3600000

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)


    // Log Changer widget inittialization is required for setting setup
    // default location for log changer is {webapproot}/loglevel/change
    LogLevelChanger.init()

    // Initialization for upload progress widget
    UploadProgress.init

    //Make sure we don't put stuff in memory for uploads
    LiftRules.handleMimeFile = OnDiskFileParamHolder.apply

    // Initialization for auto complete widget
    AutoComplete.init

    // Initilization for table sorter widget
    TableSorter.init

    // Initialization for flot (charting) widget
    Flot.init

    MenuWidget.init()

    // for ajax file upload
    LiftRules.progressListener = {
      val opl = LiftRules.progressListener
      val ret: (Long, Long, Int) => Unit =
        (a, b, c) => {
          // println("progress listener "+a+" plus "+b+" "+c)
          // Thread.sleep(100) -- demonstrate slow uploads
          opl(a,b,c)
        }
      ret
    }


    //Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    S.addAround(DB.buildLoanWrapper)

    if(!Props.productionMode){
      Category.findAll().foreach(_.delete_!)
      (1 to 10).foreach{ i: Int => {
        val cat = Category.create.name("Category" + i).saveMe()
        val tags = (1 to 5).map(i => Tag.create.name("Tag" + i + "_for_" + cat.name.is).category(cat).saveMe())
      }}
    }

  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}

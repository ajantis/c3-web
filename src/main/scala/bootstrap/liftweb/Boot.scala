package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.java.sql.{Connection, DriverManager}
import org.aphreet.c3.model._
import org.aphreet.c3.apiaccess.C3Client
import net.liftweb.mapper._
import net.liftweb.http._

import js.jquery.JQuery14Artifacts

import net.liftweb.widgets.logchanger._
import net.liftweb.widgets.uploadprogress._

import org.aphreet.c3.logging.LogLevel
import net.liftweb.widgets.tablesorter.TableSorter
import org.apache.commons.httpclient.util.URIUtil
import net.liftweb.widgets.autocomplete.AutoComplete
import net.liftweb.widgets.flot.Flot


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

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
    Schemifier.schemify(true, Schemifier.infoF _, User, Group, Category,Message,UserGroup)

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
      S.param("groupname") match {
        case Full(name) => Group.find(By(Group.name,name)) match {
          case Full(group) => {
            User.currentUser match {
              case Full(user) if(user.id.is == group.owner.is) => true
              case _ => false
            }
          }
          case _ => false
        }
        case _ => false
      }
    },
    () => RedirectWithState("/index", RedirectState( () => {} ,"Not a group admin" -> NoticeType.Notice ) ) )

    // Build SiteMap
    def sitemap() = SiteMap(

      Menu("IEDisclaimer") / "ie_disclaimer" >> isIE >> Hidden,

      Menu("About") / "about" >> LocGroup("footerMenu"),

      Menu("Faq") / "faq" >> LocGroup("footerMenu"),

      Menu("Users") / "users" / "index" >> isSuperAdmin >> LocGroup("mainmenu"),

      Menu("Categories") / "categories" >> loggedIn >> LocGroup("mainmenu"),

      Menu("Groups") / "groups" >> loggedIn >> LocGroup("mainmenu"),

      Menu("Home") / "index" >> LocGroup("footerMenu") >> User.AddUserMenusAfter, // Simple menu form

      Menu("GroupOverview") / "groupsection" / "index" >> loggedIn >> Hidden,

      Menu("GroupFiles") / "groupsection" / "files" >> loggedIn >> Hidden,

      Menu("GroupFiles") / "groupsection" / "file" >> loggedIn >> Hidden,

      Menu("GroupWiki") / "groupsection" / "wiki-view" >> loggedIn >> Hidden,

      Menu("GroupWiki") / "groupsection" / "wiki-edit" >> loggedIn >> Hidden,

      Menu("GroupAdmin") / "groupsection" / "admin" >> loggedIn >> Hidden >> isGroupAdmin,

      Menu("UserEdit") / "users" / "edituser" >> loggedIn >> Hidden,

      Menu("Search") / "search" >> loggedIn >> Hidden,

      Menu("Not found") / "404" >>  Hidden,


      // VM service's menu parts
      Menu("VM Service") / "vmservice" / "index" >> loggedIn >> Hidden,
      Menu("VMS vm's overview") / "vmservice" / "view_vm" >> loggedIn >> Hidden,


      LogLevel.menu // default log level menu is located at /loglevel/change


    )

    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))



    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupFilesRewrite") {
      case RewriteRequest(
      ParsePath("group" :: groupname  :: "files" :: directory , extension, _,_), _, _) => {

        val dotExt = extension match {
          case "" => ""
          case str => "."+str
        }
        Group.find(By(Group.name,groupname)) match {
          case Full(group) => {
            // TODO if resource instance is FIle ==>> rewrite request to file page
            C3Resource.get(group,directory.mkString("/")+dotExt) match {
              case Some(resource) if(resource.isInstanceOf[File]) => {
                //RewriteResponse("download" :: groupname :: "files" :: (directory.mkString("/")+dotExt).split("/").toList)
                RewriteResponse("groupsection" :: "file" :: Nil, Map("groupname" -> URIUtil.decode(groupname,"UTF-8"),"groupdirectory" -> (directory.mkString("/") + dotExt),"filepath" -> (directory.mkString("/") + dotExt), "rewrite" -> "groupFiles"))
              }
              case Some(resource) => RewriteResponse("groupsection" :: "files" :: Nil, Map("groupname" -> groupname,"groupdirectory" -> directory.mkString("/"), "rewrite" -> "groupFiles"))
              case _ => RewriteResponse("404" :: Nil)
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
        () =>
          for {
            stream <- tryo(new java.io.ByteArrayInputStream(
              try{
                C3Client().getNodeData(groupname + "/" + filePath.reverse.tail.reverse.mkString("/") + "/" +
                  filePath.last + {
                  extension match {
                    case "" => ""
                    case ext => "." + ext
                  }
                })
              }
              catch {
                case e: Exception => {
                  e.printStackTrace
                  S.notice("No file found!")
                  S.redirectTo("/group/"+groupname+"/files/"+filePath.reverse.tail.reverse.mkString("/"))
                }
              }))
            if null ne stream
          } yield StreamingResponse(stream, () => stream.close,
            stream.available, List("Content-Type" ->
              C3Client().getResourseContentType(groupname +
                "/" + filePath.reverse.tail.reverse.mkString("/") + "/" +
                filePath.last + {
                extension match {
                  case "" => ""
                  case ext => "." + ext
                }
              }
              )),
            Nil,200)
    }

    // for ajax file upload
    import org.aphreet.c3.lib.FileUpload
    LiftRules.dispatch.append(FileUpload)

    import org.aphreet.c3.openid.OpenIDVendor

    // dispatches for open ID support login
    LiftRules.dispatch.append(OpenIDVendor.dispatchPF)
    LiftRules.snippets.append(OpenIDVendor.snippetPF)

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

    LiftRules.statelessRewrite.prepend(NamedPF("IENotSupportedDisclaimerIndexRewrite") {
      case RewriteRequest(
      ParsePath("index" :: Nil , _, _,_), _, req)  if( req.userAgent.map(_.contains("MSIE")) openOr(false) ) =>
        RewriteResponse(
          "ie_disclaimer" :: Nil
        )
    })


    LiftRules.statelessRewrite.prepend(NamedPF("IENotSupportedDisclaimerLoginRewrite") {
      case RewriteRequest(
      ParsePath("user_mgt" :: _ :: Nil , _, _,_), _, req)  if( req.userAgent.map(_.contains("MSIE")) openOr(false) ) =>
        RewriteResponse(
          "ie_disclaimer" :: Nil
        )
    })

    // VM service rewrites
    LiftRules.statelessRewrite.prepend(NamedPF("VMServiceViewVMRewrite") {
      case RewriteRequest(
      ParsePath("vmservice" :: "vm" :: vmName :: Nil , _, _,_), _, _) =>
        RewriteResponse(
          "vmservice" :: "view_vm" :: Nil, Map("vmName" -> URIUtil.decode(vmName,"UTF-8"),"rewrite" -> "vmOverview" )
        )
    })


    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)



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

    /*
    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent)) */

    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}



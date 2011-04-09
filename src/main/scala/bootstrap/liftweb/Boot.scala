package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.java.sql.{Connection, DriverManager}
import org.aphreet.c3.model._
import org.aphreet.c3.apiaccess.C3Client
import net.liftweb.mapper._
import java.net.URLEncoder

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

    // where to search snippet
    LiftRules.addToPackages("org.aphreet.c3")
    Schemifier.schemify(true, Schemifier.infoF _, User, Group, Category,Message,UserGroup)

    val loggedIn = If(() => User.loggedIn_?,
                  () => RedirectResponse("/user_mgt/login"))

    val isSuperAdmin = If(() => {if(!User.currentUser.isEmpty) User.currentUser.open_!.superUser.is else false},
                            ()=> RedirectResponse("/user_mgt/login"))

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
                  () => RedirectResponse("/index"))

    // Build SiteMap
    def sitemap() = SiteMap(

      Menu("About") / "about" >> LocGroup("footerMenu"),

      Menu("Faq") / "faq" >> LocGroup("footerMenu"),

      Menu("Users") / "users" / "index" >> isSuperAdmin >> LocGroup("mainmenu"),

      Menu("Categories") / "categories" >> loggedIn >> LocGroup("mainmenu"),

      Menu("Groups") / "groups" >> loggedIn >> LocGroup("mainmenu"),

      Menu("Home") / "index" >> LocGroup("footerMenu") >> User.AddUserMenusAfter, // Simple menu form

      Menu("GroupOverview") / "groupsection" / "index" >> loggedIn >> Hidden,

      Menu("GroupFiles") / "groupsection" / "files" >> loggedIn >> Hidden,

      Menu("GroupWiki") / "groupsection" / "wiki-view" >> loggedIn >> Hidden,

      Menu("GroupWiki") / "groupsection" / "wiki-edit" >> loggedIn >> Hidden,

      Menu("GroupAdmin") / "groupsection" / "admin" >> loggedIn >> Hidden >> isGroupAdmin,

      Menu("UserEdit") / "users" / "edituser" >> loggedIn >> Hidden,

      Menu("File upload") / "file_upload" >> loggedIn >> Hidden,

      Menu("Search") / "search" >> loggedIn >> Hidden


      // Menu with special Link
      //Menu(Loc("Static", Link(List("static"), true, "/static/index"),"Static Content"))


    )

    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))



    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupFilesRewrite") {
        case RewriteRequest(
            ParsePath("group" :: groupname  :: "files" :: directory , _, _,_), _, _) =>
            RewriteResponse(
                //C3Client().getNodeMetadata()      // TODO !!
                "groupsection" :: "files" :: Nil, Map("groupname" -> groupname,"groupdirectory" -> directory.mkString("/"), "rewrite" -> "groupFiles")
            )
    })
    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupOverviewRewrite") {
        case RewriteRequest(
            ParsePath("group" :: groupname  :: Nil , _, _,_), _, _) =>
            RewriteResponse(
                "groupsection" ::  "index" :: Nil, Map("groupname" -> groupname)
            )
    })
    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupWikiRewrite") {
        case RewriteRequest(
            ParsePath("group" :: groupname  :: "wiki" :: pagename :: Nil , _, _,_), _, _) =>
            RewriteResponse(
                "groupsection" ::  "wiki-view" :: Nil, Map("groupname" -> groupname, "pagename" -> pagename)
            )
    })

    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupOverviewRewriteWikiMain") {
        case RewriteRequest(
            ParsePath("group" :: groupname  :: "wiki" :: Nil , _, _,_), _, _) =>
            RewriteResponse(
                "groupsection" ::  "wiki-view" :: Nil, Map("groupname" -> groupname, "pagename" -> "Main")
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
                   C3Client().getNodeData(groupname + "/" + filePath.reverse.tail.reverse.mkString("/") + "/" + URLEncoder.encode(filePath.last) + {
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
                                                  C3Client().getResourseContentType(groupname + "/" + filePath.reverse.tail.reverse.mkString("/") + "/" + URLEncoder.encode(filePath.last) + {
                                                                                        extension match {
                                                                                          case "" => ""
                                                                                          case ext => "." + ext
                                                                                        }
                                                                                     }
                                                  )),
                                                    Nil,200)
    }



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

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}

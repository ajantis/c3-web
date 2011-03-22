package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import org.aphreet.c3.model._

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

    // Build SiteMap
    def sitemap() = SiteMap(
      Menu("Home") / "index" >> User.AddUserMenusAfter, // Simple menu form

      Menu("Groups") / "groups" >> loggedIn,

      Menu("GroupsView") / "viewgroup" >> loggedIn >> Hidden,

      Menu("Users") / "users" >> loggedIn,

      Menu("File upload") / "file_upload" >> loggedIn >> Hidden,


      // Menu with special Link
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),"Static Content"))


    )

    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))



    LiftRules.statelessRewrite.prepend(NamedPF("ParticularGroupViewRewrite") {
        case RewriteRequest(
            ParsePath("group" :: groupname  :: directory , _, _,_), _, _) =>
            RewriteResponse(
                "viewgroup" :: Nil, Map("groupname" -> groupname,"groupdirectory" -> directory.mkString("/"))  // Use webapp/viewgroup.html
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

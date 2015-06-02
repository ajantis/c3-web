package org.aphreet.c3.snippet.users.snippet

import net.liftweb.common.{ Box, Full, Logger }
import net.liftweb.sitemap.Loc.{ Link, LinkText }
import net.liftweb.util.Helpers._
import org.aphreet.c3.loc.ItemRewriteLoc
import org.aphreet.c3.model.User
import org.aphreet.c3.snippet.users.UserPageData
import org.aphreet.c3.util.helpers.UserHelper

import scala.xml.Text

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
object UserPage extends ItemRewriteLoc[User, UserPageData] {

  override lazy val pathList = pathPrefix ++ List("index")
  override val name = "User"
  override val pathPrefix = "users" :: Nil

  override def title = Text(currentValue.map(_.user.shortName).openOr("User"))

  override def text = new LinkText[UserPageData](text = v => Text(v.user.shortName))

  override def link = new Link[UserPageData](pathList) {
    override def pathList(value: UserPageData): List[String] = pathPrefix ::: value.user.id.is.toString :: Nil
  }
  override def getItem(id: String) = User.find(id)
  override def wrapItem(userBox: Box[User]) = userBox.map(UserPageData(_))
  override def canonicalUrl(data: UserPageData) = {
    Full((pathPrefix ::: List(data.user.id.is.toString)).mkString("/", "/", ""))
  }

  def isAccessiblePage(page: UserPageData): Boolean = true
}

class UserPage(data: UserPageData) extends UserHelper {
  private val logger = Logger(classOf[UserPage])

  def view = {
    toCssBindings(data.user) &
      ".user_group *" #> data.user.groups.toList.map {
        group =>
          ".name *" #> group.name.is &
            ".group_link [href]" #> group.createLink
      }
  }
}


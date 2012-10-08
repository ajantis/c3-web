package org.aphreet.c3.snippet.user

import org.aphreet.c3.model.User
import org.aphreet.c3.loc.ItemRewriteLoc
import xml.Text
import net.liftweb.sitemap.Loc.{Link, LinkText}
import net.liftweb.common.{Logger, Full, Box}

/**
 * @author Dmitry Ivanov (mailto: Dmitry.Ivanov@reltio.com)
 *         Reltio, Inc.
 */
object UserPage extends ItemRewriteLoc[User, UserPageData] {

  override val name = "User"
  override def title = Text(currentValue.map(_.user.shortName).openOr("User"))
  override def text = new LinkText[UserPageData](text = v => Text(v.user.email.is))

  override val pathPrefix = "user" :: Nil
  override lazy val pathList = pathPrefix ++ List("index")
  override def link = new Link[UserPageData](pathList){
    override def pathList(value: UserPageData): List[String] = pathPrefix ::: value.user.id.is.toString :: Nil
  }
  override def getItem(id: String) = User.find(id)
  override def wrapItem(userBox: Box[User]) = userBox.map(UserPageData(_))
  override def canonicalUrl(data: UserPageData) = {
    Full((pathPrefix:::List(data.user.id.is.toString)).mkString("/","/",""))
  }
}

class UserPage(data: UserPageData) {
  private val logger = Logger(classOf[UserPage])

}


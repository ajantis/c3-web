package org.aphreet.c3.snippet.groups.snippet

import org.aphreet.c3.model.{UserGroup, Group}
import net.liftweb.common.{Logger, Box}
import xml.Text
import org.aphreet.c3.loc.SuffixLoc
import org.aphreet.c3.snippet.groups.{AbstractGroupPageLoc, GroupPageData}
import net.liftweb.sitemap.Loc.Link
import net.liftweb.util.BindHelpers._
/**
 * Created with IntelliJ IDEA.
 * User: Serjk
 * Date: 19.01.13
 * Time: 20:49
 * To change this template use File | Settings | File Templates.
 */
object GroupPageMembers extends AbstractGroupPageLoc[GroupPageData] with SuffixLoc {
  override val name = "Members"
  override val pathPrefix = "groups" :: Nil
  override val pathSuffix = "members" ::  Nil
  override def getItem(id: String) = Group.find(id)
  override def wrapItem(groupBox: Box[Group]) = groupBox.map(new GroupPageData(_))

  override def link = {
    new Link[GroupPageData](pathPrefix ++ pathSuffix){
    override def pathList(value: GroupPageData): List[String] = pathPrefix ::: value.group.id.is.toString :: Nil ::: pathSuffix
      }
  }
}
class GroupPageMembers(data: GroupPageData) extends GroupPageHelpers{
  override lazy val group = data.group
  override lazy val activeLocId = "members"
  def listUser = {
    val members = group.users.all
    //val  v = UserGroup.findAll()
    ".ListGroupUser *" #> members.map(user =>{
      "a" #> user.shortName &
        "a [href]" #> user.createLink
    })

  }
}

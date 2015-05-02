package org.aphreet.c3.snippet.groups.snippet

import com.ifunsoftware.c3.access.C3System
import net.liftweb.common.Box
import net.liftweb.sitemap.Loc.Link
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.loc.SuffixLoc
import org.aphreet.c3.model.Group
import org.aphreet.c3.service.groups.GroupService
import org.aphreet.c3.snippet.groups.{ AbstractGroupPageLoc, GroupPageData }
import org.aphreet.c3.util.helpers.GroupPageHelper

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 */
object GroupPageMembers extends AbstractGroupPageLoc[GroupPageData] with SuffixLoc[Group, GroupPageData] {
  override val name = "Members"
  override val pathPrefix = "groups" :: Nil
  override val pathSuffix = "members" :: Nil
  override def getItem(id: String) = Group.find(id)
  override def wrapItem(groupBox: Box[Group]) = groupBox.map(new GroupPageData(_))

  override def link = {
    new Link[GroupPageData](pathPrefix ++ pathSuffix) {
      override def pathList(value: GroupPageData): List[String] = pathPrefix ::: value.group.id.is.toString :: Nil ::: pathSuffix
    }
  }
}

class GroupPageMembers(data: GroupPageData) extends GroupPageHelper {

  lazy val c3 = inject[C3System].open_!
  lazy val groupService = inject[GroupService].open_!

  override lazy val group = data.group
  override lazy val activeLocId = "members"

}

package org.aphreet.c3.snippet.groups.snippet

import com.ifunsoftware.c3.access.C3System
import net.liftweb.common.{ Box, Full }
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.sitemap.Loc.{ Link, LinkText }
import net.liftweb.util.BindHelpers._
import org.aphreet.c3.lib.DependencyFactory
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.model.{User, Group}
import org.aphreet.c3.service.groups.GroupService
import org.aphreet.c3.snippet.LiftMessages
import org.aphreet.c3.snippet.groups.{ AbstractGroupPageLoc, GroupPageData }
import org.aphreet.c3.util.helpers.GroupPageHelper

import scala.xml.{NodeSeq, Text}

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
object GroupPage extends AbstractGroupPageLoc[GroupPageData] {

  override lazy val pathList = pathPrefix ++ List("index")
  override val name = "Group"
  override val pathPrefix = "groups" :: Nil

  override def title = Text(currentValue.map(_.group.name.is).openOr("Group"))

  override def text = new LinkText[GroupPageData](text = v => Text(v.group.name.is))

  override def link = new Link[GroupPageData](pathList) {
    override def pathList(value: GroupPageData): List[String] = pathPrefix ::: value.group.getId :: Nil
  }
  override def getItem(id: String) = Group.findById(id)
  override def wrapItem(groupBox: Box[Group]) = groupBox.map(new GroupPageData(_))
  override def canonicalUrl(data: GroupPageData) = {
    Full((pathPrefix ::: List(data.group.getId)).mkString("/", "/", ""))
  }
}

class GroupPage(data: GroupPageData) extends GroupPageHelper {
  override lazy val group = data.group
  override lazy val activeLocId = "about"
  lazy val c3 = inject[C3System].open_!
  lazy val groupService = DependencyFactory.inject[GroupService].open_!

  def info = {
    val status = if (group.isOpen) "Public" else "Private"
    val background = if (group.isOpen) "btn-info" else "btn-warning"
    val groupTags = group.getTags
    ".tags_group" #> groupTags.map((tag: String) => {
      ".tags_group *" #> tag
    }) &
      ".GroupOwner *" #> group.owner.obj.map(_.shortName).openOr("N/A") &
      ".GroupOwner [href]" #> group.owner.obj.map(_.createLink) &
      ".GroupName *" #> group.name.is &
      ".GroupAccess *" #> status &
      ".GroupAccess [class+]" #> background &
      ".GroupDescription *" #> group.getDescription &
      ".send_request [onclick]" #> SHtml.ajaxInvoke(() => sendRequest())

  }
  def sendRequest(): JsCmd = {
    val user = User.currentUserUnsafe
    if(group.users.contains(user)){
      LiftMessages.ajaxNotice(user.niceName + " already in group " + group.name.is) &
        JsCmds.Replace(group.getId, NodeSeq.Empty)
    }
      else{
      groupService.addUsersToApproveListGroup(group, Iterable(user))
      LiftMessages.ajaxNotice(user.niceName + " is add to approve list of group " + group.name.is) &
        JsCmds.Replace(group.getId, NodeSeq.Empty)
    }
  }
}

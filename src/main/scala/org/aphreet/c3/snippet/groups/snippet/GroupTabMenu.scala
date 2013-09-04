package org.aphreet.c3.snippet.groups.snippet

import net.liftweb.util.BindHelpers._
import net.liftweb.http.{RequestVar, S}
import net.liftweb.common.{Failure, Full}
import net.liftweb.util.{PassThru, CssSel}
import xml.NodeSeq
import org.aphreet.c3.model.{User, Group}

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
class GroupTabMenu {

  type GroupTabsFunc = String => List[(String, GroupTab)]

  object tabs extends RequestVar[GroupTabsFunc](defaultTabs)

  private def defaultTabs(groupId: String): List[(String, GroupTab)] =
    List("about" -> AboutTab(groupId),
      "files" -> FilesTab(groupId),
      "messages" -> MessagesTab(groupId)
//      "members" -> MembersTab(groupId)
  )

  def render: CssSel = {
    val activeTab = S.attr("active")
    val groupId = S.attr("group_id")
    val group = groupId.flatMap(id => Group.find(id))

    (User.currentUser, group) match {
      case (Full(user), Full(g)) => {
        if (user.email.is == g.owner.obj.map(_.email).open_!.is ||user.superUser.is)
          tabs.set(groupId => defaultTabs(groupId) ::: List("settings" -> SettingsTab(groupId)))
      }
      case _ =>
    }
    def tabMenu(id: String, active: String) = {
      "li" #> tabs.get(id).map{
        case (key, tab) =>
          "a *" #> tab.name &
          "a [href]" #> tab.path &
          "li [class+]" #> (if(key == active) "active" else "")
      }
    }

    val cssSel = for{
      active <- activeTab ?~ "Active tab is undefined!"
      groupId <- groupId ?~ "Current group id is undefined"
    } yield tabMenu(groupId, active)

    lazy val empty = "* *" #> NodeSeq.Empty

    cssSel match {
      case Full(c) => c
      case f: Failure => S.error(f.msg); empty
      case _ => empty
    }

  }
}

// consider to move tab definitions to specific Group Locs -- as paths depend on locs
sealed abstract class GroupTab(val name: String, val path: String)
case class AboutTab(groupId: String) extends GroupTab("About", "/groups/" + groupId)
case class FilesTab(groupId: String) extends GroupTab("Files", "/groups/" + groupId + "/files/")
case class MessagesTab(groupId: String) extends GroupTab("Messages", "/groups/" + groupId + "/messages")
case class MembersTab(groupId: String) extends GroupTab("Members", "/groups/" + groupId + "/members")
case class SettingsTab(groupId: String) extends GroupTab("Settings", "/groups/" + groupId + "/settings")
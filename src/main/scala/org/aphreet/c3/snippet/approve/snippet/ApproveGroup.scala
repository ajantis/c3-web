package org.aphreet.c3.snippet.approve.snippet

import com.ifunsoftware.c3.access.C3System
import net.liftweb.common.{Failure, Full}
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.util.Helpers._
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.lib.{DependencyFactory, NotificationManagerRef}
import org.aphreet.c3.model.{Group, User}
import org.aphreet.c3.service.groups.GroupService
import org.aphreet.c3.service.notifications.ApproveGroupMsg
import org.aphreet.c3.service.notifications.NotificationManagerProtocol.CreateNotification
import org.aphreet.c3.snippet.LiftMessages
import org.aphreet.c3.util.helpers.AdminPageHelper

import scala.xml.NodeSeq

//import com.ifunsoftware.c3.access.C3NotFoundException

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
class ApproveGroup extends AdminPageHelper {

  override lazy val activeLocId = "groupApprove"

  lazy val groupService = DependencyFactory.inject[GroupService]
    .openOrThrowException("Group service should be always here")
  lazy val notificationManager = inject[NotificationManagerRef]
    .openOrThrowException("Notification manager should be here").actorRef

  def render = {
    val groupsToApprove = Group.findAll().filter(_.isApproved!=true)

    ".list_group_approve" #> groupsToApprove.map {
      group: Group => {
        val owner = User.find(group.owner).openOrThrowException(s"No owner for ${group.name}")

        def approveGroup(): JsCmd = {

          group.getGroupC3 match {
            case Full(groupFileNode) =>
              // we don't need to create a C3 mapping for this group, it already exists
              processGroupApproval(group, owner) &
                LiftMessages.ajaxNotice(s"Group ${group.name} is approved")
            case _                   =>
              // we need to create a C3 mapping first and then create a group
              createAndApprove(group, owner)
          }
        }

        val groupTags = group.tags.get
        val groupTagsList = if(groupTags != null && !groupTags.isEmpty )
                              groupTags.split(",").toList else Nil
          ".tags_group"              #> groupTagsList.map { ".tags_group *" #> _ } &
          ".list_group_approve [id]" #> group.getId &
          ".group_name *"            #> group.name &
          ".group_description *"     #> group.description &
          ".group_owner *"           #> owner.niceName&
          ".approve_group [onclick]" #> SHtml.ajaxInvoke(() => approveGroup())
      }
    }
  }

  private def processGroupApproval(group: Group, owner: User): JsCmd = {
    group.isApproved(true).save
    notificationManager ! CreateNotification(ApproveGroupMsg(group, owner.id.is))
    JsCmds.Replace(group.getId, NodeSeq.Empty)
  }

  private def createAndApprove(group: Group, owner: User): JsCmd = {

    groupService.createGroup(group, group.tags, group.description) match {
      case Full(g) =>
        processGroupApproval(group, owner) & LiftMessages.ajaxNotice(s"Group ${g.name} is created and approved")

      case other   =>
        val reason = other match {
          case Failure(msg, _, _) => msg
          case _                  => null
        }
        LiftMessages.ajaxError(s"Group is not created. Reason: $reason")
    }
  }
}

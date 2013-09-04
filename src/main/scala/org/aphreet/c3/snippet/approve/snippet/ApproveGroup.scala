package org.aphreet.c3.snippet.approve.snippet

import net.liftweb.util.Helpers._
import org.aphreet.c3.model.{User, Group}
import net.liftweb.mapper.{NotBy, By}
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.common.{Failure, Empty, Full}
import scala.xml.NodeSeq
import org.aphreet.c3.snippet.LiftMessages
import org.aphreet.c3.lib.{NotificationManagerRef, DependencyFactory}
import org.aphreet.c3.service.groups.GroupService
import org.aphreet.c3.lib.DependencyFactory._
import net.liftweb.common.Full
import org.aphreet.c3.service.notifications.NotificationManagerProtocol.CreateNotification
import org.aphreet.c3.service.notifications.{ApproveGroupMsg, AddedToGroupMsg}

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
class ApproveGroup {

  lazy val groupService = DependencyFactory.inject[GroupService]
    .openOrThrowException("Group service should be always here")
  lazy val notificationManager = inject[NotificationManagerRef]
    .openOrThrowException("Notification manager should be here").actorRef

  def render = {
    val groupsToApprove = Group.findAll(By(Group.isApprove, false))

    ".list_group_approve" #> groupsToApprove.map {
      group: Group => {
        val owner = User.find(group.owner).openOrThrowException(s"No owner for ${group.name}")

        def approveGroup(): JsCmd = {
          group.getGroupC3 match {
            case Full(groupFileNode)             =>
              // we don't need to create a C3 mapping for this group, it already exists
              processGroupApproval(group, owner) &
              LiftMessages.ajaxNotice(s"Group ${group.name} is approved")

            case Failure("File not found", _, _) =>
              // we need to create a C3 mapping first and then create a group
              createAndApprove(group, owner)


            case Failure(msg, _, _)              =>
              LiftMessages.ajaxError(msg)
          }
        }

        val groupTags = group.getTags

        ".tags_group"              #> groupTags.map { ".tags_group *" #> _ } &
        ".list_group_approve [id]" #> group.id &
        ".group_name *"            #> group.name &
        ".group_description *"     #> group.description &
        ".group_owner *"           #> owner.niceName&
        ".approve_group [onclick]" #> SHtml.ajaxInvoke(()=>approveGroup())

      }
    }
  }

  private def processGroupApproval(group: Group, owner: User): JsCmd = {
    group.isApprove(true).save
    notificationManager ! CreateNotification(ApproveGroupMsg(group, owner.id.is))

    JsCmds.Replace(group.id.is.toString, NodeSeq.Empty)
  }

  private def createAndApprove(group: Group, owner: User): JsCmd = {
    groupService.createGroup(group, group.tags, group.description) match {
      case Full(g) =>
        processGroupApproval(group, owner) & LiftMessages.ajaxNotice(s"Group ${g.name} is approved and created")

      case Failure(msg, _, _) =>
        LiftMessages.ajaxError(msg)

      case _                  =>
        LiftMessages.ajaxError("Group is not approved")
    }
  }
}

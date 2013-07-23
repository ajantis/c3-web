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

  lazy val groupService = DependencyFactory.inject[GroupService].open_!
  lazy val notificationManager = inject[NotificationManagerRef].open_!.actorRef

  def render = {

    val listApproveGroup = Group.findAll().filter(_.isApprove!=true)

    ".list_group_approve"#> listApproveGroup.map(group=>{

      val owner = User.find(group.owner).openOrThrowException("None Owner")

      def approveGroup():JsCmd = {

        group.getGroupC3 match {
          case Full(groupC3) =>{
            group.isApprove(true).save
            notificationManager ! CreateNotification(ApproveGroupMsg(group,owner.id.is))
            JsCmds.Replace(group.id.is.toString, NodeSeq.Empty)&
            LiftMessages.ajaxNotice("Exists group "+ group.name +" is approve")
          }
          case Failure("File not found",_, _) => groupService.createGroup(group, group.tags, group.description) match {
            case Full(g) =>
              group.isApprove(true).save
              notificationManager ! CreateNotification(ApproveGroupMsg(group,owner.id.is))
              JsCmds.Replace(group.id.is.toString, NodeSeq.Empty)&
              LiftMessages.ajaxNotice(g.name +" is create and approve")

            case Failure(msg, _, _) => LiftMessages.ajaxError(msg)
            case _ => LiftMessages.ajaxError("Group is not approve")
          }
          case Failure(msg, _, _) => LiftMessages.ajaxError(msg)
        }

      }

      val groupTags = group.getTags
      ".tags_group" #> groupTags.map((tag: String) => {
        ".tags_group *" #> tag
      }) &
      ".list_group_approve [id]"#> group.id &
      ".group_name *" #> group.name &
      ".group_description *" #> group.description &
      ".group_owner *" #> owner.niceName&
      ".approve_group [onclick]"#> SHtml.ajaxInvoke(()=>approveGroup())

    }

    )
  }
}

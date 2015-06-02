package org.aphreet.c3.service.notifications

import org.aphreet.c3.model.{ NotificationType, Group, User }
import xml.NodeSeq
import com.ifunsoftware.c3.access.fs.C3File
import NotificationType._

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
abstract class NotifyMsg(val recipientId: Long, val title: String, val message: NodeSeq, val notifyType: NotificationType)

case class AddedToGroupMsg(group: Group, override val recipientId: Long) extends NotifyMsg(
  recipientId = recipientId,
  title = TemplateManager.addedGroupTitle(group.name),
  message = TemplateManager.addedGroupTemplate(group.name, group.createLink),
  notifyType = NotificationType.AddedToGroup)

case class RejectedFromGroupMsg(group: Group, override val recipientId: Long) extends NotifyMsg(
  recipientId = recipientId,
  title = TemplateManager.rejectedFromGroupTitle(group.name),
  message = TemplateManager.rejectedFromGroupTemplate(group.name, group.createLink),
  notifyType = NotificationType.RejectedFromGroup)

case class ApproveGroupMsg(group: Group, override val recipientId: Long) extends NotifyMsg(
  recipientId = recipientId,
  title = TemplateManager.approveGroupTitle(group.name),
  message = TemplateManager.approveGroupTemplate(group.name, group.createLink),
  notifyType = NotificationType.ApproveGroup)

case class FileMetaProcessedMsg(file: C3File, override val recipientId: Long) extends NotifyMsg(
  recipientId = recipientId,
  title = TemplateManager.fileMetaProcessedTitle(file.name),
  message = TemplateManager.fileMetaProcessedTemplate(file.name, file.fullname),
  notifyType = NotificationType.FileMetaUpdated)

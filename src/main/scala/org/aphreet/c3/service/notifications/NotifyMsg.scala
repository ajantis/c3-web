package org.aphreet.c3.service.notifications

import org.aphreet.c3.model.{Group, NotificationType, User}
import xml.NodeSeq
import com.ifunsoftware.c3.access.fs.C3File

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
abstract class NotifyMsg(val recipient: User, val title: String, val message: NodeSeq, val notifyType: NotificationType.Value)

case class AddedToGroupMsg(group: Group, override val recipient: User) extends NotifyMsg(
  recipient = recipient,
  title = TemplateManager.addedGroupTitle(group.name.is),
  message = TemplateManager.addedGroupTemplate(group.name.is, group.createLink),
  notifyType = NotificationType.AddedToGroup)

case class FileMetaProcessedMsg(file: C3File, override val recipient: User) extends NotifyMsg(
  recipient = recipient,
  title = TemplateManager.fileMetaProcessedTitle(file.name),
  message  = TemplateManager.fileMetaProcessedTemplate(file.name, file.fullname),
  notifyType = NotificationType.FileMetaUpdated
)
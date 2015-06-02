package org.aphreet.c3.service.notifications

import xml.NodeSeq
import net.liftweb.http.S
import net.liftweb.util.Helpers._

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
object TemplateManager {
  private val addedGroupTemplatePath: List[String] = List("templates", "_added_to_group_message_template_en")
  private val fileMetaProcessedTemplatePath: List[String] = List("templates", "_file_meta_processed_msg_template_en")
  private val approveGroupTemplatePath: List[String] = List("templates", "_approve_group_message_template_en")
  private val rejectedFromGroupTemplatePath: List[String] = List("templates", "_approve_group_message_template_en")

  private val addedGroupMessageTemplate: NodeSeq = S.runTemplate(addedGroupTemplatePath).getOrElse(<div>Template is not found</div>)
  private val fileMetaProcessedMessageTemplate: NodeSeq = S.runTemplate(fileMetaProcessedTemplatePath).getOrElse(<div>Template is not found</div>)
  private val approveGroupMessageTemplate: NodeSeq = S.runTemplate(approveGroupTemplatePath).getOrElse(<div>Template is not found</div>)
  private val rejectedFromGroupMessageTemplate: NodeSeq = S.runTemplate(rejectedFromGroupTemplatePath).getOrElse(<div>Template is not found</div>)

  private val addedGroupMessageTitle: String = S.?("added.to.group.msg.title")
  private val fileMetaProcessedMessageTitle: String = S.?("file.meta.processed.msg.title")
  private val approveGroupMessageTitle: String = S.?("approve.group.msg.title")
  private val rejectedFromGroupMessageTitle: String = S.?("reject.from.group.msg.title")

  def addedGroupTitle(groupName: String) = addedGroupMessageTitle.replace("{group.name}", groupName)

  def approveGroupTitle(groupName: String) = approveGroupMessageTitle.replace("{group.name}", groupName)

  def rejectedFromGroupTitle(groupName: String) = rejectedFromGroupMessageTitle.replace("{group.name}", groupName)

  def fileMetaProcessedTitle(fileName: String) = fileMetaProcessedMessageTitle.replace("{file.name}", fileName)

  def addedGroupTemplate(groupName: String, groupLink: String): NodeSeq = {
    (".group_name *" #> groupName & ".group_link [href]" #> groupLink).apply(addedGroupMessageTemplate)
  }

  def rejectedFromGroupTemplate(groupName: String, groupLink: String): NodeSeq = {
    (".group_name *" #> groupName & ".group_link [href]" #> groupLink).apply(rejectedFromGroupMessageTemplate)
  }

  def approveGroupTemplate(groupName: String, groupLink: String): NodeSeq = {
    (".group_name *" #> groupName & ".group_link [href]" #> groupLink).apply(approveGroupMessageTemplate)
  }

  def fileMetaProcessedTemplate(fileName: String, fileLink: String): NodeSeq = {
    (".file_name *" #> fileName & ".file_link [href]" #> fileLink).apply(fileMetaProcessedMessageTemplate)
  }
}
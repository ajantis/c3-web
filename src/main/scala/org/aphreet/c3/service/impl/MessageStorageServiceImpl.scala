package org.aphreet.c3.service.impl

import org.aphreet.c3.service.{MessageStorageException, MessageStorageService}
import org.aphreet.c3.common.C3Loggable
import org.aphreet.c3.model.{Group, Message}
import org.aphreet.c3.lib.DependencyFactory._
import com.ifunsoftware.c3.access.{DataStream, C3System}
import net.liftweb.common.Box
import com.ifunsoftware.c3.access.fs.C3Directory
import org.apache.commons.io.IOUtils
import net.liftweb.util.TimeHelpers.{now, time}
import net.liftweb.util.Helpers
import MessageStorageServiceImpl._
import io.Source

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
class MessageStorageServiceImpl extends MessageStorageService with C3Loggable{

  private val metaTags = Set(MSG_CREATOR_META, MSG_DATE_META)

  lazy val c3 = inject[C3System].open_!

  @throws(classOf[MessageStorageException])
  override def findAll(group: Group): Traversable[Message] = {
    val messagesRoot = getGroupMessagesRoot(group)

    if(messagesRoot.isEmpty){
      warn("Message root of group " + group.name.is + " is not found!")
      List()
    }
    else {
      val messages = for {
        root <- messagesRoot.toList
        file <- root.children(embedChildrenData = true, embedChildMetaData = metaTags).filter(!_.isDirectory).map(_.asFile)
      } yield {
        val md = file. metadata
        Message(group.id.is.toString,
          Box(md.get("x-c3-msg-creator")).openOr("N/A"),
          time(Box(md.get("x-c3-msg-date")).map(_.split("-").head.toLong).openOr(now.getTime)),
          file.versions.head.getData.readContentAsString)
      }

      messages
    }
  }

  @throws(classOf[MessageStorageException])
  override def save(msg: Message): Box[Message] = {
    for{
        group <- msg.group ?~ "Group message belongs to is not defined!"
        root <- getGroupMessagesRoot(group)
    } yield {
      val tagsMap = buildTagsMap(CreatorTag(msg.author.map(_.id.is.toString).openOr("N/A")), DateTag(msg.creationDate.getTime.toString))
      root.createFile(msg.creationDate.getTime.toString + "-" + Helpers.md5(msg.content).take(5), tagsMap, DataStream(msg.content))
      msg
    }
  }

  @throws(classOf[MessageStorageException])
  override def delete(msg: Message): Boolean = {
    val result = for{
      group <- msg.group
      root <- getGroupMessagesRoot(group)
      msgFileName = msg.creationDate.getTime.toString + "-" + Helpers.md5(msg.content).take(5)
      file <- root.getChild(msgFileName)
    } yield {
      c3.deleteFile(file.fullname)
      msg
    }

    !result.isEmpty
  }

  protected def getGroupMessagesRoot(group: Group): Box[C3Directory] = {
    val root = c3.getFile("/").asDirectory
    val groupRoot = root.getChild(group.name.is).filter(_.isDirectory).map(_.asDirectory)
    groupRoot.flatMap(_.getChild(GROUP_MESSAGES_ROOT).filter(_.isDirectory).map(_.asDirectory))
  }

  protected def buildTagsMap(tags: MsgMDTag*): Map[String, String] = {
    tags.map(t => t.name -> t.value).toMap
  }

}

object MessageStorageServiceImpl{

  val GROUP_MESSAGES_ROOT = "messages"
  val MSG_CREATOR_META = "x-c3-msg-creator"
  val MSG_DATE_META = "x-c3-msg-date"

  def apply: MessageStorageService = new MessageStorageServiceImpl
}

sealed abstract class MsgMDTag(val name: String, val value: String)

final case class CreatorTag(creator: String) extends MsgMDTag(name = MSG_CREATOR_META, value = creator)
final case class DateTag(dateStr: String) extends MsgMDTag(name = MSG_DATE_META, value = dateStr)
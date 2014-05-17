package org.aphreet.c3.service.groups.messages.impl

import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.model.Group
import org.aphreet.c3.lib.DependencyFactory._
import JournalStorageServiceImpl._
import org.aphreet.c3.lib.metadata.Metadata._
import net.liftweb.util.Helpers
import Helpers._
import org.aphreet.c3.service.groups.messages.{ MessageStorageException, JournalStorageService }
import com.ifunsoftware.c3.access.{ DataStream, C3System }
import com.ifunsoftware.c3.access.C3System._
import net.liftweb.common.{ Full, Box }
import com.ifunsoftware.c3.access.fs.C3Directory
import org.aphreet.c3.service.journal.{ JournalEntity, Message, EventType, Event }

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
class JournalStorageServiceImpl extends JournalStorageService with C3Loggable {

  private val metaTags = Set(MSG_CREATOR_META, TAGS_META)
  private val eventMeta = Set(EVENT_CREATOR_META, EVENT_DATE_META, EVENT_PATH_META, EVENT_TYPE_META)
  private val MSG_FILE_PREFIX = "msg-"
  private val EVENT_FILE_PREFIX = "event-"

  lazy val c3 = inject[C3System].open_!

  @throws(classOf[MessageStorageException])
  override def findMsgAll(group: Group): Traversable[Message] = {
    val messagesRoot = getGroupMessagesRoot(group)

    if (messagesRoot.isEmpty) {
      warn("Message root of group " + group.name.is + " is not found!")
      List()
    } else {
      val messages = for {
        root ← messagesRoot.toList
        file ← root.children(embedChildrenData = true, embedChildMetaData = metaTags).filter(!_.isDirectory).map(_.asFile)
      } yield {
        val md = file.metadata

        val tags: List[String] = md.get(TAGS_META) match {
          case Some("") => Nil
          case Some(s)  => s.split(',').toList
          case _        => Nil
        }

        Message(group.id.is.toString,
          Box(md.get(MSG_CREATOR_META)).openOr("N/A"),
          file.versions.last.date,
          file.versions.last.getData.readContentAsString,
          messageUUID(file.name),
          tags)
      }

      messages.sortWith((cd1, cd2) => cd1.creationDate.after(cd2.creationDate))
    }
  }

  @throws(classOf[MessageStorageException])
  override def findEventAll(group: Group): Traversable[Event] = {

    val eventRoot = getGroupEventRoot(group)
    if (eventRoot.isEmpty) {
      warn("Event root of group " + group.name.is + " is not found!")
      List()
    } else {
      for {
        root ← eventRoot.toList
        file ← root.children(embedChildrenData = true, embedChildMetaData = eventMeta).filter(!_.isDirectory).map(_.asFile)
      } yield {
        val md = file.metadata
        val user = Box(md.get(EVENT_CREATOR_META)).openOr("N/A")
        val uuid = eventUUID(file.name)
        val date = file.versions.last.date
        val eventType = EventType.withName(md.get(EVENT_TYPE_META).get)
        val path = md.get(EVENT_PATH_META).getOrElse("")

        Event(group.id.is.toString, user, uuid, date, eventType, path)
      }
    }
  }

  @throws(classOf[MessageStorageException])
  def findAll(group: Group): Traversable[JournalEntity] = {
    val msg = findMsgAll(group)
    val events = findEventAll(group)
    (msg ++ events).toList.sortBy {
      case m: Message => m.creationDate
      case e: Event   => e.creationDate
    }
  }

  @throws(classOf[MessageStorageException])
  override def save(msg: Message): Box[Message] = {
    for {
      group ← msg.group ?~ "Group message belongs to is not defined!"
      root ← getGroupMessagesRoot(group)
    } yield {
      val tagsMap = buildTagsMap(CreatorTag(msg.author.map(_.id.is.toString).openOr("N/A")), MessageTags(msg.tags))
      root.createFile(messageFileName(msg), tagsMap, DataStream(msg.content))
      msg
    }
  }

  @throws(classOf[MessageStorageException])
  override def save(event: Event): Box[Event] = {

    for {
      group ← event.group ?~ "Group event belongs to is not defined!"
      root ← getGroupMessagesRoot(group)
    } yield {
      val metadataMap = buildEventMetadata(event)
      root.createFile(eventFileName(event), metadataMap, DataStream(""))
      event
    }
  }

  @throws(classOf[MessageStorageException])
  override def delete(msg: Message): Boolean = {
    val result = for {
      group ← msg.group
      root ← getGroupMessagesRoot(group)
      msgFileName = messageFileName(msg)
      file ← root.getChild(msgFileName)
    } yield {
      c3.deleteFile(file.fullname)
      msg
    }

    !result.isEmpty
  }

  private def getDirectory(path: String): Box[C3Directory] = {
    tryo {
      c3.getFile(path)
    }.filter(_.isDirectory).map(_.asDirectory)
  }

  protected def getGroupMessagesRoot(group: Group): Box[C3Directory] = {
    val path = "/" + group.id.is.toString + "/" + GROUP_MESSAGES_ROOT + "/"
    getDirectory(path)
  }

  protected def getGroupEventRoot(group: Group): Box[C3Directory] = {

    val pathEvent = "/" + group.id.is.toString + "/" + GROUP_EVENTS_ROOT + "/"

    val directory = getDirectory(pathEvent)

    directory match {
      case Full(d) => Full(d)
      case _ =>
        val path = "/" + group.id.is.toString + "/"
        val directoryGroup = c3.getFile(path)
        directoryGroup.asDirectory.createDirectory(GROUP_EVENTS_ROOT, Map())
        Thread.sleep(1000) //wait create directory on c3-storage
        getDirectory(pathEvent)
    }
  }

  protected def buildTagsMap(tags: MsgMDTag*): Map[String, String] = {
    tags.map(t => t.name -> t.value).toMap
  }

  protected def buildEventMetadata(event: Event): Map[String, String] = {
    Map(EVENT_CREATOR_META -> event.author.map(_.id.is.toString).openOr("N/A"),
      EVENT_DATE_META -> event.creationDate.toString,
      EVENT_TYPE_META -> event.eventType.toString,
      EVENT_PATH_META -> event.path)
  }

  protected def messageFileName(msg: Message) = MSG_FILE_PREFIX + msg.uuid

  protected def messageUUID(fileName: String) = fileName.replace(MSG_FILE_PREFIX, "")

  protected def eventFileName(event: Event) = EVENT_FILE_PREFIX + event.uuid

  protected def eventUUID(eventName: String) = eventName.replace(EVENT_FILE_PREFIX, "")

}

object JournalStorageServiceImpl {
  val GROUP_MESSAGES_ROOT = "messages"

  val GROUP_EVENTS_ROOT = "events"

  private lazy val storage = new JournalStorageServiceImpl

  def apply: JournalStorageService = storage
}

sealed abstract class MsgMDTag(val name: String, val value: String)

final case class CreatorTag(creator: String) extends MsgMDTag(name = MSG_CREATOR_META, value = creator)

final case class MessageTags(tags: List[String]) extends MsgMDTag(name = TAGS_META, value = tags.mkString(","))

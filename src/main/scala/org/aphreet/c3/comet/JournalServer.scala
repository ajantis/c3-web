package org.aphreet.c3.comet

import net.liftweb.http.ListenerManager
import net.liftweb.actor.LiftActor
import net.liftweb.common.Logger

import org.aphreet.c3.service.journal.{JournalEntity, Message, EventType, Event}
import EventType._
import org.aphreet.c3.model.{Group, User}
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.service.groups.messages.JournalStorageService

import java.util

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
class JournalServer(val group: Group) extends LiftActor with ListenerManager {

  private val logger = Logger(classOf[JournalServer])
  private lazy val journalService = inject[JournalStorageService].open_!

  override def lowPriority = {
    case JournalServerMsg(user, messageGroup, content, tags) if content.length > 0 =>
      val msg = Message(group.getId, user.id.is.toString, content, util.UUID.randomUUID().toString, tags)
      logger.debug("Received a message: " + msg + ". Saving...")
      journalService.save(msg)
      updateListeners()

    case JournalServerComment(user, messageGroup, content, tags, parentId) if content.length > 0 =>
      val msg = Message(group.getId, user.id.is.toString, content, util.UUID.randomUUID().toString, tags, Some(parentId))
      logger.debug(s"Received a comment: $msg. Saving...")
      journalService.saveComment(msg)
      updateListeners()

    case JournalServerEvent(user, groupEvent, eventType, path) =>
      val event = Event(group.getId, user.id.is.toString, util.UUID.randomUUID().toString, eventType, path)
      journalService.save(event)
      updateListeners()

    case msg@_ =>
      logger.error(s"Unknown message received from comet actor: $msg")
  }

  def createUpdate = JournalServerUpdate(journalService.findAll(group).toList)

}

object MessageServerFactory {

  private val logger = Logger("MessageServerFactory")

  // message servers simple cache =)
  private var messageServers = Map[String, JournalServer]()

  def apply(group: Group): JournalServer = {
    messageServers.get(group.name.is) match {
      case Some(ms) => ms
      case _ => addJournalServer(group)
    }
  }

  private def addJournalServer(group: Group): JournalServer = {
    logger.info("Creating new journal server for group " + group.name.is)
    val journalServer = new JournalServer(group)
    val newMap = messageServers + (group.name.is -> journalServer)
    messageServers.synchronized {
      messageServers = newMap
    }
    journalServer
  }
}

case class JournalServerMsg(user: User, group: Group, msg: String, tags: List[String])

case class JournalServerComment(user: User, group: Group, msg: String, tags: List[String], parentId: String)

case class JournalServerUpdate(journal: List[JournalEntity])

case class JournalServerEvent(user: User, group: Group, event: EventType, path: String)


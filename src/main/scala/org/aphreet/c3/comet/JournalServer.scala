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
    case MessageServerMsg(user, messageGroup, content, tags) if content.length > 0 =>
      val msg = Message(group.id.is.toString, user.id.is.toString, content, util.UUID.randomUUID().toString, tags)
      logger.debug("Received a message: " + msg + ". Saving...")
      journalService.save(msg)
      updateListeners()
    case EventServer(user, groupEvent, eventType, path) =>
      val event = Event(group.id.is.toString, user.id.is.toString, util.UUID.randomUUID().toString, eventType, path)
      journalService.save(event)
      updateListeners()
    case msg @ _ =>
      logger.error(s"Unknown message received from comet actor: $msg")
  }

  def createUpdate = JournalServerUpdate(journalService.findAll(group).take(20).toList)

}

object MessageServerFactory{

  private val logger = Logger("MessageServerFactory")

  // message servers simple cache =)
  private var messageServers = Map[String, JournalServer]()

  def apply(group: Group): JournalServer = {
    messageServers.get(group.name.is) match {
      case Some(ms) => ms
      case _ => addMessageServer(group)
    }
  }

  private def addMessageServer(group: Group): JournalServer = {
    logger.info("Creating new message server for group " + group.name.is)
    val ms = new JournalServer(group)
    val newMap = messageServers + (group.name.is -> ms)
    messageServers.synchronized{
      messageServers = newMap
    }
    ms
  }
}

case class MessageServerMsg(user: User, group: Group, msg: String, tags: List[String])
case class JournalServerUpdate(journal: List[JournalEntity])

case class EventServer(user: User, group: Group, event: EventType, path:String)


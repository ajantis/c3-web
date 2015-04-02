package org.aphreet.c3.service.journal

import java.util
import net.liftweb.common.Box
import org.aphreet.c3.model.{ Group, User }
import EventType._
import net.liftweb.util.TimeHelpers

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
case class Event(groupId: String, authorId: String, uuid: String, creationDate: util.Date, eventType: EventType, path: String) extends JournalEntity {
  lazy val author: Box[User] = User.find(authorId)
  lazy val group: Box[Group] = Group.findById(groupId)

  override def toString = {
    val builder = new StringBuilder("Event{")
    builder.append("authorId=").append(authorId).
      append(", groupId=").append(groupId).
      append(", eventDate=").append(creationDate.getTime).
      append(", typeEvent=").append(eventType).
      append(", pathResource=").append(path).
      append("}")

    builder.toString()
  }
}
object Event {
  def apply(groupId: String, authorId: String, uuid: String, eventType: EventType, path: String) =
    new Event(groupId, authorId, uuid: String, TimeHelpers.now, eventType, path)
}

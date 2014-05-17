package org.aphreet.c3.service.groups.messages

import org.aphreet.c3.model.{ User, Group }
import java.util
import org.aphreet.c3.util.C3Exception
import net.liftweb.common.Box
import org.aphreet.c3.service.journal.{ JournalEntity, Message, Event }

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
trait JournalStorageService {

  @throws(classOf[MessageStorageException])
  def findMsgAll(group: Group): Traversable[Message]

  @throws(classOf[MessageStorageException])
  def findEventAll(group: Group): Traversable[Event]

  @throws(classOf[MessageStorageException])
  def findAll(group: Group): Traversable[JournalEntity]

  @throws(classOf[MessageStorageException])
  def save(msg: Message): Box[Message]

  @throws(classOf[MessageStorageException])
  def save(msg: Event): Box[Event]

  @throws(classOf[MessageStorageException])
  def delete(msg: Message): Boolean
}

class MessageStorageException extends C3Exception


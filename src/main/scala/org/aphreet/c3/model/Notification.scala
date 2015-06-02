package org.aphreet.c3.model

import net.liftweb.mapper._
import xml.{ NodeSeq, XML }
import net.liftweb.util.TimeHelpers

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class Notification extends LongKeyedMapper[Notification] with IdPK {

  def getSingleton = Notification

  object title extends MappedString(this, 256)

  object body extends MappedString(this, 1024) {
    def toXml: NodeSeq = XML.loadString(this.is)
  }

  object recipient extends MappedLongForeignKey(this, User)

  object notificationType extends MappedEnum(this, NotificationType)

  object isRead extends MappedBoolean(this) {
    override def defaultValue = false
  }

  object created extends MappedDateTime(this) {
    override def defaultValue = TimeHelpers.now
  }

  def createLink: String = "/notifications/" + this.id.is
}

object Notification extends Notification with LongKeyedMetaMapper[Notification] {
  override def dbTableName = "notifications"
  def findByRecipient(user: User) = this.findAll(By(Notification.recipient, user.id.is))

  def totalByRecipient(user: User) = this.count(By(Notification.recipient, user.id.is))

  def unreadByRecipient(user: User) = this.count(By(Notification.recipient, user.id.is),
    By(Notification.isRead, false))
}

object NotificationType extends Enumeration {
  type NotificationType = Value

  val AddedToGroup = Value("added_to_group")
  val RejectedFromGroup = Value("rejected_from_group")
  val ApproveGroup = Value("approve_group")
  val FileMetaUpdated = Value("file_meta_updated")
}
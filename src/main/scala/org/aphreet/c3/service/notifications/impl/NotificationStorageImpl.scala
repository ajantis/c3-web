package org.aphreet.c3.service.notifications.impl

import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.model.{NotificationType, Notification, User}
import org.aphreet.c3.service.notifications.{NotifyMsg, NotificationStorage}
import net.liftweb.util.FieldError

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class NotificationStorageImpl extends NotificationStorage with C3Loggable{
  def saveNotification(notifyMsg: NotifyMsg){
    val newNotification = Notification.create.recipient(notifyMsg.recipient).title(notifyMsg.title).body(notifyMsg.message.toString())
    newNotification.validate match {
      case Nil => newNotification.save()
      case xs: List[FieldError] => logger.error("Cannot submit a notification. Reason: " + xs)
    }
  }

  def getNotificationsForUser(recipient: User): List[Notification] = Notification.findByRecipient(recipient)

  def markAsRead(notification: Notification): Notification = {
    if (!notification.isRead.is){
      notification.isRead(true).saveMe()
    } else {
      logger.error("Notification is already marked as read! " + notification)
      notification
    }
  }
}

object NotificationStorageImpl{
  def create: NotificationStorage = new NotificationStorageImpl
}

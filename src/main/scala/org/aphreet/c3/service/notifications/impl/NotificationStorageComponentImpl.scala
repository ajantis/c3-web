package org.aphreet.c3.service.notifications.impl

import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.model.{Notification, User}
import org.aphreet.c3.service.notifications.{NotifyMsg, NotificationStorageComponent}
import net.liftweb.util.FieldError

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
trait NotificationStorageComponentImpl extends NotificationStorageComponent{

  override val notificationStorage: NotificationStorage = new NotificationStorageImpl

  private[this] class NotificationStorageImpl extends NotificationStorage with C3Loggable{
    def saveNotification(notifyMsg: NotifyMsg){
      val newNotification = Notification.create.notificationType(notifyMsg.notifyType).recipient(notifyMsg.recipientId).title(notifyMsg.title).body(notifyMsg.message.toString())
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
}


package org.aphreet.c3.service.notifications

import impl.NotificationStorageImpl
import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.model.{User, Notification}
import org.aphreet.c3.service.notifications.NotificationManagerProtocol.{GetUserNotifications, CreateNotification, MarkAsRead}
import akka.actor.Actor

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class NotificationManager extends Actor with C3Loggable{
  private val notificationService = new NotificationStorageImpl

  def receive = {
    case CreateNotification(notifyMessage)  => notificationService.saveNotification(notifyMessage)

    case MarkAsRead(notification)           => notificationService.markAsRead(notification)

    case GetUserNotifications(user)         => sender ! notificationService.getNotificationsForUser(user)
  }

}

object NotificationManagerProtocol {

  abstract sealed class NotificationManagerCmd

  case class CreateNotification(notifyMessage: NotifyMsg)

  case class MarkAsRead(notification: Notification)

  case class GetUserNotifications(user: User)
}


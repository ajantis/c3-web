package org.aphreet.c3.service.notifications

import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.model.{ User, Notification }
import org.aphreet.c3.service.notifications.NotificationManagerProtocol.{ GetUserNotificationStats, GetUserNotifications, CreateNotification, MarkAsRead }
import akka.actor.Actor

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
trait NotificationManager extends Actor with C3Loggable {
  self: NotificationStorageComponent =>

  def receive = {
    case CreateNotification(notifyMessage) => notificationStorage.saveNotification(notifyMessage)

    case MarkAsRead(notification)          => notificationStorage.markAsRead(notification)

    case GetUserNotifications(user)        => sender ! notificationStorage.getNotificationsForUser(user)

    case GetUserNotificationStats(user)    => sender ! notificationStorage.getNotificationStatsForUser(user)
  }
}

object NotificationManagerProtocol {

  abstract sealed class NotificationManagerCmd

  case class CreateNotification(notifyMessage: NotifyMsg)

  case class MarkAsRead(notification: Notification)

  case class GetUserNotifications(user: User)

  case class GetUserNotificationStats(user: User)
}


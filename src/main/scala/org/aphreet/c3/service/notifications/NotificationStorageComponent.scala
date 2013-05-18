package org.aphreet.c3.service.notifications

import org.aphreet.c3.model.Notification
import org.aphreet.c3.model.User

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
trait NotificationStorageComponent{

  val notificationStorage: NotificationStorage

  trait NotificationStorage {
    def saveNotification(notifyMsg: NotifyMsg)

    def getNotificationsForUser(recipient: User): List[Notification]

    def markAsRead(notification: Notification): Notification

    def getNotificationStatsForUser(recipient: User): NotificationStats
  }
}

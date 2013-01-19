package org.aphreet.c3.snippet.notifications.snippet

import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.service.NotificationService
import org.aphreet.c3.model.{Notification, User}
import net.liftweb.util.BindHelpers._

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class NotificationListPage {
  private val notificationService = inject[NotificationService].open_!

  def list = {
    val currentUser = User.currentUser
    val notifications: List[Notification] = currentUser.map(notificationService.getNotificationsForUser _).getOrElse(Nil)

    ".notification *" #> notifications.map { notification =>
      ".title *" #> notification.title.is &
      ".link [href]" #> notification.createLink
    }
  }
}
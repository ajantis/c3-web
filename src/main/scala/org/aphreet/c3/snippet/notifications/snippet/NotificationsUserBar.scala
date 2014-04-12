package org.aphreet.c3
package snippet.notifications.snippet

import service.notifications.NotificationStats
import service.notifications.NotificationManagerProtocol.GetUserNotificationStats

import lib.DependencyFactory._
import lib.NotificationManagerRef
import util.helpers.AkkaAwareSnippet
import model.User

import net.liftweb.common.Box
import net.liftweb.util.BindHelpers._

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.pattern.ask

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class NotificationsUserBar extends AkkaAwareSnippet {

  val notificationManager = inject[NotificationManagerRef].
    openOrThrowException("Notification manager is not available").actorRef

  def render = {
    val unread: Box[Long] = notificationStats.map(Await.result(_, 2 seconds)).map(_.unread)

    ".unread_count *" #> unread.getOrElse(0L) &
      ".unread_count [class+]" #> (if (unread.map(_ > 0).getOrElse(false)) "badge-important" else "")
  }

  protected def notificationStats: Box[Future[NotificationStats]] = {
    User.currentUser.map(notificationManager ? GetUserNotificationStats(_)).map(_.mapTo[NotificationStats])
  }

}
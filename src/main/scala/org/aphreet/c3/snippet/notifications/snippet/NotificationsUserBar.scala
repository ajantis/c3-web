package org.aphreet.c3.snippet.notifications.snippet

import scala.concurrent.{duration, Future, Await}
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.lib.NotificationManagerRef
import org.aphreet.c3.model.User
import org.aphreet.c3.service.notifications.NotificationManagerProtocol.GetUserNotificationStats
import akka.pattern.ask
import org.aphreet.c3.service.notifications.NotificationStats
import net.liftweb.common.Box
import duration._
import net.liftweb.util.BindHelpers._
import org.aphreet.c3.util.helpers.AkkaAwareSnippet


/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class NotificationsUserBar extends AkkaAwareSnippet{

  val notificationManager = inject[NotificationManagerRef].
    openOrThrowException("Notification manager is not available").actorRef

  def render = {
    val unread: Box[Long] = notificationStats.map(Await.result(_, 2 seconds)).map(_.unread)

    ".unread_count *" #>  unread.getOrElse(0L) &
    ".unread_count [class+]" #> (if(unread.map(_ > 0).getOrElse(false)) "badge-important" else "")
  }

  protected def notificationStats: Box[Future[NotificationStats]] = {
    User.currentUser.map(notificationManager ? GetUserNotificationStats(_)).map(_.mapTo[NotificationStats])
  }

}
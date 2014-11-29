package org.aphreet.c3.snippet.notifications.snippet

import akka.pattern.ask
import net.liftweb.common.Box
import net.liftweb.util.BindHelpers._
import net.liftweb.util.PassThru
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.lib.NotificationManagerRef
import org.aphreet.c3.model.{Notification, User}
import org.aphreet.c3.service.notifications.NotificationManagerProtocol.GetUserNotifications
import org.aphreet.c3.util.helpers.{AkkaAwareSnippet, DateTimeHelper}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, duration}
import scala.language.postfixOps
import scala.xml.{NodeSeq, Text}

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class NotificationListPage extends AkkaAwareSnippet {

  val notificationManager = inject[NotificationManagerRef].open_!.actorRef

  def list = {
    val currentUser = User.currentUser
    val notifications = {
      val result: Box[Future[List[Notification]]] = currentUser.map(notificationManager ? GetUserNotifications(_))
        .map(_.mapTo[List[Notification]])

      result
    }

    def asNotViewed(xml: NodeSeq): NodeSeq = {
      <strong>{ xml }</strong>
    }

    if (notifications.isEmpty)
      ".notification" #> Text("You have no notifications yet.")
    else
      ".notification *" #> notifications.map(Await.result(_, 2 seconds)).openOr(Nil).reverse.map { notification =>
        ".created *" #> DateTimeHelper.todayTimeOrPastDate(notification.created.is) &
          ".link [href]" #> notification.createLink &
          ".title *" #> notification.title.is andThen
          (notification.isRead.is match {
            case false => {
              ".title *" #> asNotViewed _ &
                ".created *" #> asNotViewed _ &
                ".source *" #> asNotViewed _
            }
            case true => PassThru
          })
      }
  }
}
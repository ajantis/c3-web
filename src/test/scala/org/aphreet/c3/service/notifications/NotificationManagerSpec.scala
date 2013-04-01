package org.aphreet.c3.service.notifications

import org.scalatest.{BeforeAndAfterAll, WordSpec}
import org.scalatest.matchers.ShouldMatchers
import akka.testkit.TestKit
import akka.actor.{Props, ActorSystem}
import org.aphreet.c3.model.{Notification, User}
import concurrent.duration
import duration._
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import scala.language.implicitConversions
import scala.language.postfixOps

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
class NotificationManagerSpec extends TestKit(ActorSystem("TestActorSystem"))
  with WordSpec
  with BeforeAndAfterAll
  with ShouldMatchers
  with MockitoSugar{

  import NotificationManagerProtocol._

  val user = User.create
  val notificationManagerRef = system.actorOf(Props(new NotificationManager with NotificationStorageComponentMock))
  val notifications = List(Notification.create.body("test body1"), Notification.create.body("test body2"))

  "A NotificationManager" should {
    "return all notifications for this user from storage" in {
      within(1 second) {
        notificationManagerRef.tell(GetUserNotifications(user), testActor)
        expectMsg(notifications)
      }
    }
  }

  trait NotificationStorageComponentMock extends NotificationStorageComponent{
    override val notificationStorage: NotificationStorage = mock[NotificationStorage]
    when(notificationStorage.getNotificationsForUser(user)).thenReturn(notifications)
  }

}
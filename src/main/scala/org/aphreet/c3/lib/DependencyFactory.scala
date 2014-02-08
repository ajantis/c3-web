package org.aphreet.c3.lib

import net.liftweb._
import http._
import util._
import org.aphreet.c3.apiaccess.C3
import org.aphreet.c3.service.groups.impl.GroupServiceImpl
import org.aphreet.c3.service.groups.messages.impl.MessageStorageServiceImpl
import akka.actor.{ActorRef, Props, ActorSystem}
import org.aphreet.c3.service.metadata.MetadataService
import org.aphreet.c3.service.notifications.impl.NotificationStorageComponentImpl

/**
 * A factory for generating new instances of Date.  You can create
 * factories for each kind of thing you want to vend in your application.
 * An example is a payment gateway.  You can change the default implementation,
 * or override the default implementation on a session, request or current call
 * stack basis.
 */
object DependencyFactory extends Factory {

  implicit object akkaSystem extends FactoryMaker(actorSystem _)

  implicit object time extends FactoryMaker(Helpers.now _)

  implicit object c3 extends FactoryMaker(C3.apply _ )

  implicit object messageService extends FactoryMaker(MessageStorageServiceImpl.apply _)

  implicit object groupService extends FactoryMaker(GroupServiceImpl.apply _)

  implicit object metadataService extends FactoryMaker(MetadataServiceRef.apply(metadataServiceRef))

  implicit object notificationManager extends FactoryMaker(NotificationManagerRef.apply(notificationManagerRef))

  /**
   * objects in Scala are lazily created.  The init()
   * method creates a List of all the objects.  This
   * results in all the objects getting initialized and
   * registering their types with the dependency injector
   */
  private def init() {
    List(time, c3, messageService, groupService, akkaSystem, metadataService, notificationManager)
  }

  // Akka stuff
  private lazy val actorSystem = bootAkkaSystem
  private val metadataServiceName = "MetadataService"
  private val notificationManagerName = "NotificationManager"

  //TODO rewrite this on actorSystem.actorSelection
  private def metadataServiceRef = actorSystem.actorFor("/user/" + metadataServiceName)
  private def notificationManagerRef = actorSystem.actorFor("/user/" + notificationManagerName)

  private def bootAkkaSystem: ActorSystem = {
    val actorSystem = ActorSystem("C3WebSystem")

    val notificationManager = actorSystem.actorOf(Props.create(classOf[NotificationStorageComponentImpl]), name = notificationManagerName)
    actorSystem.actorOf(Props.create(classOf[MetadataService], notificationManager), name = metadataServiceName)

    actorSystem
  }

  init()

}

case class MetadataServiceRef(actorRef: ActorRef)
case class NotificationManagerRef(actorRef: ActorRef)
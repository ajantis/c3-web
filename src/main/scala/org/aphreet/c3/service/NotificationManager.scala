package org.aphreet.c3.service

import net.liftweb.actor.LiftActor
import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.lib.DependencyFactory._

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
object NotificationManager extends LiftActor with C3Loggable{
  private val notificationService = inject[NotificationService].open_!

  def messageHandler = {
    case notifyMessage: NotifyMsg => notificationService.sendNotification(notifyMessage)
    case msg => logger.error("Unkhown message is received: " + msg)
  }

}
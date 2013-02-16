package org.aphreet.c3.util.helpers

import akka.util.duration._
import akka.util.{Timeout, Duration}
import org.aphreet.c3.lib.DependencyFactory
import akka.actor.ActorSystem

/**
 * @author Dmitry Ivanov (dmitry.ivanov@reltio.com)
 *         Reltio, Inc.
 */
trait AkkaAwareSnippet {

  val akkaSystem = DependencyFactory.inject[ActorSystem].open_!

  implicit val timeout: Timeout = 2 seconds

}
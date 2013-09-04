package org.aphreet.c3.util.helpers

import concurrent.duration
import duration._
import org.aphreet.c3.lib.DependencyFactory
import akka.actor.ActorSystem
import akka.util.Timeout
import scala.languageFeature.postfixOps

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
trait AkkaAwareSnippet {

  val akkaSystem = DependencyFactory.inject[ActorSystem].open_!

  implicit val timeout: Timeout = 2 seconds

}
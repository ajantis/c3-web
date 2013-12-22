package org.aphreet.c3
package util.helpers

import lib.DependencyFactory

import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor.ActorSystem
import akka.util.Timeout

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
trait AkkaAwareSnippet {

  val akkaSystem = DependencyFactory.inject[ActorSystem].open_!

  implicit val timeout: Timeout = 2 seconds

}
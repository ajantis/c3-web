package org.aphreet.c3.service.metadata

import akka.actor.Actor
import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.service.metadata.MetadataServiceProtocol._

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class MetadataServiceWorker extends Actor with C3Loggable{
  def receive = {
    case ProcessC3Resource(res) => {
      logger.debug("Received a C3 resource " + res.address + " for processing.")
      // TODO process c3 resource: get keywords, send notification to owner, remove s4-meta-flag
    }
  }
}
package org.aphreet.c3.service.metadata

import akka.actor.{ ActorRef, Actor }
import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.service.metadata.MetadataServiceProtocol._
import org.aphreet.c3.lib.metadata.Metadata
import Metadata._
import net.liftweb.common.{ Box, Failure, Full }
import org.aphreet.c3.service.notifications.NotificationManagerProtocol.CreateNotification
import org.aphreet.c3.model.{ Group, User }
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import com.ifunsoftware.c3.access.{ MetadataRemove, C3System }
import com.ifunsoftware.c3.access.fs.C3File
import org.aphreet.c3.service.notifications.FileMetaProcessedMsg

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class MetadataServiceWorker(c3system: C3System, notificationManager: ActorRef) extends Actor with C3Loggable {
  def receive = {
    case ProcessC3Resource(res) => {
      logger.debug("Received a C3 resource " + res.address + " for processing.")
      if (res.metadata.get(S4_PROCESSED_FLAG_META).isDefined) {
        res.metadata.get(TAGS_META) match {
          case Some(tags) => {
            logger.debug("Got tags for resource " + res.address + ": " + tags)
            val ownerIdBox = Box(res.metadata.get(OWNER_ID_META).flatMap(asLong(_)))
            var groupId = (res.metadata.get(GROUP_ID_META)).toString
            if (groupId == null) groupId = "No group id in metadata provided";
            (for {
              ownerId ← ownerIdBox ?~ "No owner id in metadata provided"
              owner ← User.find(By(User.id, ownerId)) ?~ ("User with id " + ownerId + " is not found!")
              group ← Group.findById(groupId) ?~ ("Group with id " + groupId + " is not found!")

            } yield (owner, group)) match {
              case Full((owner: User, group: Group)) => {
                // TODO we do 2 requests to C3... make it in 1
                fsPath(res.address).map(c3system.getFile _) match {
                  case Some(f: C3File) => {
                    notificationManager ! CreateNotification(FileMetaProcessedMsg(f, owner.id.is))
                    res.update(MetadataRemove(List(S4_PROCESSED_FLAG_META))) // updating metadata with s4-meta flag removed
                  }
                  case _ => logger.error("Resource " + res.address + " is not found in virtual FS... Skipping")
                }
              }
              case Failure(msg, _, _) => logger.debug(msg)
              case _                  => logger.error("Something unexpected happen.")
            }
          }
          case _ => logger.debug("Resource " + res.address + " has no tags assigned. Skipping...")
        }
      } else {
        logger.error("C3 resource has no " + S4_PROCESSED_FLAG_META + " flag in metadata. Skipping...")
      }
    }
  }

  private def fsPath(ra: String): Option[String] = {
    c3system.getResource(ra, List(FS_PATH_META)).systemMetadata.get(FS_PATH_META)
  }
}
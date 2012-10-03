package org.aphreet.c3.snippet

import org.aphreet.c3.lib.group.GroupContextAware
import net.liftweb.http.S
import net.liftweb.util.PassThru
import org.aphreet.c3.common.C3Loggable
import xml.NodeSeq
import net.liftweb.common.{Empty, Full}
import org.aphreet.c3.comet.UpdateGroupMapping

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
class CurrentGroupController extends GroupContextAware with C3Loggable{

  def render = {
    logger.debug("Current group is: " + S.param("groupname").openOr("N/A"))
    S.param("groupname").foreach(setCurrentGroupName(_))
    PassThru
  }

  def putCometMessenger(xml: NodeSeq): NodeSeq = {
    S.param("groupname") match {
      case Full(name) => {
        val id= "group-" + name + "-messages-log-comet-actor"
        logger.debug("Using CometActor with name: %s".format(id))

        for (sess <- S.session) sess.sendCometActorMessage("GroupMessagesLog", Full(id), UpdateGroupMapping(name))

        <div class={"lift:comet?type=GroupMessagesLog;name=" + id + ";ul_id=main_ul_id;li_id=main_li_id;input_container_id=input_container"}>
          {xml}
        </div>
      }
      case _ =>
        NodeSeq.Empty
    }
  }

}

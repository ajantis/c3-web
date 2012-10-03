package org.aphreet.c3.lib.group

import net.liftweb.http.SessionVar
import org.aphreet.c3.model.Group
import net.liftweb.mapper.By
import net.liftweb.common.{Full, Empty, Box}

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
trait GroupContextAware {

  protected def currentGroup: Box[Group] = for {
    name <- currentGroupNameVar.get
    group <- Group.findByName(name)
  } yield group

  protected def setCurrentGroupName(name: String){
    currentGroupNameVar.set(Full(name))
  }

}

object currentGroupNameVar extends SessionVar[Box[String]](Empty)

package org.aphreet.c3.snippet

import net.liftweb.util.PassThru
import net.liftweb.http.{S, SessionVar}
import net.liftweb.common.{Empty, Box}
import org.aphreet.c3.model.Group
import net.liftweb.mapper.By

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */

object CurrentGroupVar extends SessionVar[Box[Group]](Empty)

class CurrentGroup {

  val groupRegex = "/groups/([^/]*)".r

  def render = {
    val group = for{
      req <- S.request
      uri = req.uri
      groupName <- groupRegex.findFirstIn(uri)
      group <- Group.find(By(Group.name, groupName))
    } yield group

    CurrentGroupVar.set(group)

    PassThru
  }
}

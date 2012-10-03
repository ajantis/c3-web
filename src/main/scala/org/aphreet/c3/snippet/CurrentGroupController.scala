package org.aphreet.c3.snippet

import org.aphreet.c3.lib.group.GroupContextAware
import net.liftweb.http.S
import net.liftweb.util.PassThru

/**
 * @author Dmitry Ivanov (mailto: Dmitry.Ivanov@reltio.com)
 *         Reltio, Inc.
 */
class CurrentGroupController extends GroupContextAware{
  def render = {
    System.out.println(S.param("groupname").openOr("N/A"))
    S.param("groupname").foreach(setCurrentGroupName(_))
    PassThru
  }
}

package org.aphreet.c3.snippet

import net.liftweb.util.Helpers._
import net.liftweb.common.Box
import java.util.Date
import org.aphreet.c3.lib.DependencyFactory
import org.aphreet.c3.model.Group

/**
 * Created with IntelliJ IDEA.
 * User: Serjk
 * Date: 21.04.12
 * Time: 14:28
 * To change this template use File | Settings | File Templates.
 */

class FancyBox {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  // bind the date into the element with id "time"
  def howdy = "#time *" #> date.map(_.toString)

  def getGroup: List[Group] = {
    Group.findAll()
  }
}

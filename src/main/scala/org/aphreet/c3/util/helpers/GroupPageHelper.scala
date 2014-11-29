package org.aphreet.c3.util.helpers

import net.liftweb.util.BindHelpers._
import org.aphreet.c3.model.Group

trait GroupPageHelper {
  val group: Group
  val activeLocId: String

  def embedTabMenu = {
    "* *" #> <lift:embed what="/groups/_group_tab_menu" active={ activeLocId } group_id={ group.id.is.toString }/>
  }
}

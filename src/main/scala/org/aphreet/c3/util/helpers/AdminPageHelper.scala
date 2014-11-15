package org.aphreet.c3.util.helpers

import net.liftweb.util.BindHelpers._

trait AdminPageHelper {
  val activeLocId: String

  def embedTabMenu = {
    "* *" #> <lift:embed what="/admin/_admin_tab_menu" active={ activeLocId }/>
  }
}

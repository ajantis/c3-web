package org.aphreet.c3.util.helpers

import org.aphreet.c3.model.Group
import net.liftweb.util.BindHelpers._

trait AdminPageHelpers {
  val activeLocId: String

  def embedTabMenu = {
    "* *" #> <lift:embed what="/admin/_admin_tab_menu" active={ activeLocId }/>
  }
}

package org.aphreet.c3.util.helpers

import net.liftweb.util.BindHelpers._
import org.aphreet.c3.model.User

import scala.xml.Text

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */

trait UserHelper {
  def toCssBindings(user: User) = {
    ".email *" #> user.email &
      ".mailto [href]" #> Text("mailto:" + user.email.is) &
      ".link [href]" #> user.createLink &
      ".name *" #> user.shortName &
      ".name [class+]" #> (if (user.superUser.is) "admin" else "")
  }
}
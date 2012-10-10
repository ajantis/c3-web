package org.aphreet.c3.snippet.user.snippet

import net.liftweb.util.BindHelpers._
import org.aphreet.c3.model.User

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */

trait UserHelpers {
  def toCssBindings(user: User) = {
    ".email *" #> user.email &
    ".link [href]" #> user.profileLink &
    ".name *" #> user.shortName &
    ".name [class+]" #> (if(user.superUser.is) "admin" else "")
  }
}
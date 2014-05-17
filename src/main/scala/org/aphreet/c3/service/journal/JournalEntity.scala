package org.aphreet.c3.service.journal

import net.liftweb.common.Box
import org.aphreet.c3.model.{Group, User}

trait JournalEntity{
  val author: Box[User]
  val group: Box[Group]
}
package org.aphreet.c3.util

import net.liftweb.util.Props
import org.aphreet.c3.model.User
import net.liftweb.mapper.By
import net.liftweb.common.{Full, Logger}

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
object DefaultAuthDataLoader {
  private val logger = Logger("DefaultAuthDataLoader")

  def init(){
    logger info "Init of DefaultAuthDataLoader..."
    createDefaultUsers()
  }

  private def createDefaultUsers(){
    val defaultAdminEmail = Props.get("default_admin_email").openOr("admin@admin.com")

    User.find(By(User.email, defaultAdminEmail)) match {
      case Full(existingUser) =>
        logger info String.format("User %s exists.", defaultAdminEmail)
      case _                  => {
        logger info String.format("Creating default user with e-mail %s...", defaultAdminEmail)
        val defaultAdminPassword = Props.get("default_admin_password").openOr("admin")
        val adminUser = User.create
          .email(defaultAdminEmail)
          .password(defaultAdminPassword)
          .validated(true)
          .firstName("Admin")
          .superUser(true)
          .enabled(true)
          .saveMe()

        logger info String.format("User %s is created.", adminUser.email.is)
      }
    }
  }
}
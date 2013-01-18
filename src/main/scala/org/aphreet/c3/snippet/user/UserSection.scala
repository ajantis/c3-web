package org.aphreet.c3.snippet.user

import net.liftweb.sitemap.Menu
import org.aphreet.c3.Section
import snippet.UserPage

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
object UserSection extends Section{
  override lazy val menus: List[Menu] = List(Menu(UserPage))
}
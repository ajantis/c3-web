package org.aphreet.c3.snippet.group

import net.liftweb.sitemap.{Menu, Loc}
import org.aphreet.c3.Section
import snippet.{GroupPageMessages, GroupPage}

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
object GroupSection extends Section{
  override lazy val menus: List[Menu] = List(Menu(GroupPage, Menu(GroupPageMessages)))
}
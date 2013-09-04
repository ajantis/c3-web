package org.aphreet.c3.snippet.groups

import org.aphreet.c3.model.{User, Group}
import org.aphreet.c3.loc.{PageData, ItemRewriteLoc}
import xml.{Text, NodeSeq}
import net.liftweb.sitemap.Loc.Link

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
trait AbstractGroupPageLoc[Data <: GroupPageData]
  extends ItemRewriteLoc[Group, Data] {
  def isAccessiblePage(page: Data): Boolean = {
    User.currentUser.map(_.superUser.is).openOr(false)||page.group.isOpen.is || User.currentUser.map(_.groups.find(_.name.is == page.group.name.is).isDefined).openOr(false)
  }
}

package org.aphreet.c3.snippet.groups

import org.aphreet.c3.model.Group
import org.aphreet.c3.loc.{PageData, ItemRewriteLoc}
import xml.{Text, NodeSeq}
import net.liftweb.sitemap.Loc.Link

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
trait AbstractGroupPageLoc[Data <: GroupPageData]
  extends ItemRewriteLoc[Group, Data] {

  // TODO Here we want to check if group is accessible by current user
  def isAccessiblePage(page: Data): Boolean = true
}

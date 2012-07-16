package org.aphreet.c3.loc

import net.liftweb.common.Full
import net.liftweb.sitemap.Loc
import xml.{NodeSeq, Text}
import net.liftweb.util.Helpers._

/**
 * Copyright iFunSoftware 2012
 * @author Dmitry Ivanov
 */

class GroupWikiLoc extends GroupLoc[GroupWikiPage]{

  def name = "wiki"

  def defaultValue = Full(GroupWikiPage("Main", "My group", false))

  private val wikiViewLink = List("wiki")
  private val wikiEditLink = List("wiki", "edit")

  def params = Nil

  val link = new Loc.Link[GroupWikiPage](List("wiki"), false){
    override def createLink(in: GroupWikiPage) = Full(
      Text("/" + (basePrefixUrl :: in.groupName :: { if(in.edit) wikiEditLink else wikiViewLink } :: urlEncode(in.wikiName) :: Nil).mkString("/")))
  }

  val text = new Loc.LinkText(calcLinkText _)

  def calcLinkText(in: GroupWikiPage): NodeSeq =
    if(in.edit) Text("Wiki edit " + in.wikiName)
    else Text("Wiki "+in.wikiName)
}
package org.aphreet.c3.loc

import net.liftweb.common.{Logger, Full}
import net.liftweb.sitemap.Loc
import xml._
import net.liftweb.util.Helpers._
import net.liftweb.util.NamedPF
import net.liftweb.http._
import java.io.StringWriter
import be.devijver.wikipedia.{SmartLink, SmartLinkResolver, Parser}
import org.aphreet.c3.lib.wiki.C3HtmlVisitor
import be.devijver.wikipedia.SmartLink.SmartLinkType
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.loc.GroupWikiPage
import org.aphreet.c3.service.WikiService
import org.aphreet.c3.snippet.GroupForm
import net.liftweb.common.Full
import scala.Some
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import org.aphreet.c3.loc.GroupWikiPage
import net.liftweb.util.BindHelpers._
import net.liftweb.common.Full
import scala.Some
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import org.aphreet.c3.loc.GroupWikiPage
import org.aphreet.c3.view.GroupNavigationUtil
import net.liftweb.common.Full
import scala.Some
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import xml.Text
import org.aphreet.c3.loc.GroupWikiPage

/**
 * Copyright iFunSoftware 2012
 * @author Dmitry Ivanov
 */

class GroupWikiLoc extends GroupLoc[GroupWikiPage]{

  lazy val wikiService = inject[WikiService].open_!
  val logger = Logger(classOf[GroupWikiLoc])

  def name = "wiki"

  def defaultValue = Full(GroupWikiPage("Main", "My group", edit = false))

  def params = Nil

  val link = new Loc.Link[GroupWikiPage](List(basePrefixUrl), true){
    override def createLink(in: GroupWikiPage) =
      Full( Text("/" + (basePrefixUrl :: in.groupName :: "wiki" :: urlEncode(in.wikiName) :: { if(in.edit) "edit" :: Nil else Nil }).mkString("/")) )
  }

  val text = new Loc.LinkText(calcLinkText _)

  def calcLinkText(in: GroupWikiPage): NodeSeq =
    if(in.edit) Text("Wiki edit " + in.wikiName)
    else Text("Wiki "+in.wikiName)

  override def snippets: SnippetTest = {
    case ("wiki", Full(wp @ GroupWikiPage(_, _, true))) =>
      editWiki(wp) _
    case ("wiki", Full(wp: GroupWikiPage)) =>
      displayWiki(wp) _
  }

  override val rewrite: LocRewrite =
    Full(NamedPF("Wiki Rewrite"){
      case RewriteRequest(ParsePath(
        basePrefixUrl() :: groupName :: "wiki" :: wikiName :: "edit" :: Nil, _, _,_),_,_) =>
          (RewriteResponse("wiki" :: Nil), GroupWikiPage(wikiName, groupName, edit = true) )
      case RewriteRequest(ParsePath(
        basePrefixUrl() :: groupName :: "wiki" :: wikiName :: Nil, _, _,_),_,_) =>
          (RewriteResponse("wiki" :: Nil), GroupWikiPage(wikiName, groupName, edit = true) )
    })


  def editWiki(wikiPage: GroupWikiPage) = {
    wikiBaseInfo(wikiPage) &
    ".actions *" #> {
      ".btn_cancel [href]" #> {"/" + List(basePrefixUrl, wikiPage.groupName, wikiSuffix, wikiPage.wikiName).mkString("/")}
    } &
    ".metadata *" #> wikiMetadataCssSel(wikiPage.metadata)
  }

  def displayWiki(wikiPage: GroupWikiPage) = {
      wikiBaseInfo(wikiPage) &
      ".content *" #> wikiPage.content.openOr(Text("Page not found")) &
      ".actions *" #> {
        ".btn_edit [href]" #> {"/" + List(basePrefixUrl, wikiPage.groupName, wikiSuffix, wikiPage.wikiName, "edit").mkString("/")}
      } &
      ".metadata *" #> wikiMetadataCssSel(wikiPage.metadata) &
      "view ^*" #> NodeSeq.Empty
  }

  protected def wikiBaseInfo(page: GroupWikiPage) = {
    ".groupname *" #> page.groupName &
    ".groupnav *" #> GroupNavigationUtil.createNavigation(page.groupName) &
    ".name *" #> page.wikiName
  }

  protected def wikiMetadataCssSel(meta: Map[_ <: String, String]) =
    meta map { md =>
      ".key *" #> md._1
      ".value *" #> md._2
    }


}
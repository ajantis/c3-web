package org.aphreet.c3.loc

import org.aphreet.c3.model.{User, Wiki, Group}
import net.liftweb.common.{Empty, Full, Box}
import xml.{NodeSeq, Text, XML}
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.service.WikiService
import java.io.StringWriter
import be.devijver.wikipedia.{SmartLink, SmartLinkResolver, Parser}
import org.aphreet.c3.lib.wiki.C3HtmlVisitor
import net.liftweb.util.BindHelpers._
import GroupWikiLoc._
import net.liftweb.http.{SHtml, RequestVar, S}
import net.liftweb.util.Helpers
import collection.mutable

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
sealed class GroupPage(groupName: String){
  lazy val group: Box[Group] = Group findByName groupName
}

case class GroupFilesPage(filePath: String, groupName: String) extends GroupPage(groupName)

case class GroupWikiPage(wikiName: String, groupName: String, edit: Boolean) extends GroupPage(groupName) {

  val resolveSmartLink = implicitly[Function2[String, String, SmartLink]]

  private lazy val wikiService = inject[WikiService]

  lazy val wiki: Box[Wiki] = wikiService.flatMap(_.getPage(groupName, wikiName))

  lazy val content = for {
    page <- wiki
  } yield XML.loadString(formatContent(page.content, groupName))

  lazy val metadata = (for {
    page <- wiki
  } yield page.metadata ).openOr(Map())

  private def formatContent(content:String, group:String): String = {
    val writer = new StringWriter
    new Parser().withVisitor(content.replaceAll("([^\r])\n", "$1\r\n"), new C3HtmlVisitor(writer, new SmartLinkResolver(){
      override def resolve(key:String): SmartLink = {
        resolveSmartLink(key, group)
      }
    }))
    writer.toString
  }

  private object pagePreview extends RequestVar[Box[String]](Empty)
  private object pageContent extends RequestVar[Box[String]](Empty)

  def toForm = {
    def saveOrUpdate(newContent: String) = () => {
      for {
        service <- wikiService ?~ "No wiki service is present!"
        page <- service.getPage(groupName, wikiName).or(Full(new Wiki(wikiName, newContent)))
      } yield service.savePage(groupName, page)
    }

    var submittedContent = ""
    import Helpers._
    def processPreview() = tryo( onError = (e: Throwable) => S.error("Failed to parse page: " + e.getMessage) ) {
      val formattedContent = formatContent(submittedContent, groupName)
      pagePreview.set(Full(formattedContent))
      pageContent.set(Full(submittedContent))
    }

    import net.liftweb.http.SHtml._

    ".content" #> textarea(wiki.map(_.content).openOr(""), submittedContent = _) &
    ".submit" #> submit("Save", saveOrUpdate(submittedContent)) &
    ".submit-preview" #> submit("Preview", processPreview _)

  }
}
package org.aphreet.c3.loc

import org.aphreet.c3.model.{Wiki, Group}
import net.liftweb.common.{Empty, Full, Box}
import org.aphreet.c3.apiaccess.C3
import xml.{NodeSeq, Text, XML}
import org.aphreet.c3.lib.DependencyFactory._
import scala.Some
import org.aphreet.c3.service.WikiService
import java.io.StringWriter
import be.devijver.wikipedia.{SmartLink, SmartLinkResolver, Parser}
import org.aphreet.c3.lib.wiki.C3HtmlVisitor
import be.devijver.wikipedia.SmartLink.SmartLinkType
import net.liftweb.http.S
import net.liftweb.util.BindHelpers._

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

  def toForm(): NodeSeq = {
    def saveOrUpdate(newContent: String) = {
      wikiService map { (service: WikiService) =>
        val page = service.getPage(groupName, wikiName).map{ w =>
          w.content = newContent
          w
        }.openOr(new Wiki(wikiName, newContent))
        service.savePage(groupName, page)
      }
    }

    import net.liftweb.http.SHtml._

    // TODO refactor
    ".content" -> textarea(editablePageContent, submittedContent = _) &
    ".preview" -> previewXml &
    ".submit" -> submit("Save", saveOrUpdate(submittedContent)) &
    ".submit-preview" -> submit("Preview", processPreview)

  }




}
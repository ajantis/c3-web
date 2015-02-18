package org.aphreet.c3.snippet

import net.liftweb.util._
import Helpers._
import org.aphreet.c3.model._
import net.liftweb.http._
import net.liftweb.http.js.{ JE, JsCmd, JsCmds }
import scala.xml.{ Unparsed, NodeSeq }
import org.aphreet.c3.lib.DependencyFactory._
import com.ifunsoftware.c3.access.C3System
import net.liftweb.util.CssSel
import java.text.SimpleDateFormat
import org.aphreet.c3.lib.metadata.Metadata
import org.aphreet.c3.util.C3Loggable
import org.aphreet.c3.lib.metadata.Metadata._
import scala.Some
import com.ifunsoftware.c3.access.SearchResultEntry
import net.liftweb.mapper.By
import net.liftweb.common.Empty

/**
 * @author Sergey Koyushev (mailto: serjk91@gmail.com)
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 */
class Search extends PaginatorSnippet[SearchResultEntry] with C3Loggable {

  private val c3 = inject[C3System].openOrThrowException("c3Storage is not accessible")
  private val dateFormat = new SimpleDateFormat("MMM dd, yyyy")

  val tagTemplate = <li class="tag"><a href="#" class="label btn-info name">Sample tag</a></li>
  var entryHtml = NodeSeq.Empty
  var noResultsHtml = NodeSeq.Empty
  var paginationHtml = NodeSeq.Empty
  var welcomeHtml = NodeSeq.Empty

  val queryInputId = "s_query"
  val queryParam = "query"
  val paginationBarId = "pagination"

  object queryString extends SessionVar[String]("")
  object firstP extends RequestVar[Long](S.param(offsetParam).map(toLong) openOr _first max 0)
  object results extends RequestVar[List[SearchResultEntry]](Nil)

  override def count = results.size

  override def itemsPerPage = 5

  override def page = results.drop(curPage * itemsPerPage).take(itemsPerPage)

  override def first = firstP.get

  /**
   * Returns XML that links to a page starting at the given record offset, if the offset is valid and not the current one.
   * @param ns The link text, if the offset is valid and not the current offset; or, if that is not the case, the static unlinked text to display
   */
  override def pageXml(newFirst: Long, ns: NodeSeq): NodeSeq =
    if (newFirst < 0 || newFirst >= count)
      <li><a href="#">{ ns }</a></li>
    else {
      val (liClass, onClick) =
        if (first == newFirst)
          ("active", "#")
        else
          ("", SHtml.ajaxCall(JE.ValById(queryInputId), s => redoSearch(newFirst, SearchQuery(s)))._2.toJsCmd)

      <li class={ liClass }><a onclick={ onClick }>{ ns }</a></li>
    }

  /**
   * Returns a URL used to link to a page starting at the given record offset.
   */
  override def pageUrl(offset: Long): String =
    appendParams(super.pageUrl(offset), List(queryParam -> queryString))

  private def redoSearch(pageNum: Long, q: SearchQuery): JsCmd = {
    firstP.set(pageNum)
    queryString.set(q.value)
    results.set(search(createC3SearchQuery(queryString)))
    val resultsHtml: NodeSeq = if (results.isEmpty)
      (".query *" #> queryString.get).apply(noResultsHtml)
    else
      page.flatMap(entry => toCss(entry).apply(entryHtml))

    JsCmds.SetHtml("results", resultsHtml) &
      JsCmds.SetHtml(paginationBarId, renderPagination(paginationHtml))
  }

  private def toCss(result: SearchResultEntry): CssSel = {
    val resource = c3.getResource(result.address, List("c3.ext.fs.path"))
    val c3Path = C3Path(result.path)
    val content = result.fragments.headOption.flatMap(_.strings.headOption.map(_.take(150)))
    val tags = resource.metadata.get(Metadata.TAGS_META).map(_.split(",").toList).getOrElse(Nil)
    val owner = resource.metadata.get(OWNER_ID_META) match {
      case Some(id) if !id.isEmpty => User.find(By(User.id, id.toLong))
      case _ => resource.metadata.get(MSG_CREATOR_META) match {
        case Some(id) if !id.isEmpty => User.find(By(User.id, id.toLong))
        case _                       => Empty
      }
    }
    var result_header_path = ""
    if (c3Path.resourceParentDir.length < 32)
      result_header_path = c3Path.resourceParentDir
    else
      result_header_path = c3Path.resourceParentDir.substring(0, 32) + "..."

    ".result_header *" #> c3Path.resourceName &
      ".result_header [href]" #> c3Path.resourceParentDir &
      ".result_header_path" #> result_header_path &
      ".result_content *" #> Unparsed(content.getOrElse("")) &
      ".result_date *" #> dateFormat.format(resource.date) &
      ".owner *" #> owner.map(_.shortName).getOrElse("Unknown") &
      ".owner [href]" #> owner.map(_.createLink) &
      ".tag *" #> {
        ".label *" #> tags
      }
  }

  private def tagToCss(tag: Tag) = {
    "li [id]" #> ("tag_" + tag.id.is) &
      ".name *" #> tag.name.is
  }

  private def renderPagination(xml: NodeSeq) = paginate(xml)

  def render = {
    initSearchParams()

    ".category *" #> Category.findAll().map {
      category =>
        {
          val tags = category.tags.toList
          "ul [id]" #> ("category_" + category.id.is + "_tags") &
            ".collapsed [data-target]" #> ("#category_" + category.id.is + "_tags") &
            ".name *" #> category.name.is &
            ".tag" #> tags.map { tag =>
              tagToCss(tag)
            }
        }
    } &
      ".search_form *" #> { (xml: NodeSeq) =>
        SHtml.ajaxForm(
          (".search_query [id]" #> queryInputId &
            ".search_query [value]" #> queryString.get &
            ".search_query" #> SHtml.onSubmit(s => {
              queryString.set(s)
            }) &
            "type=submit" #> ((xml: NodeSeq) =>
              xml ++ SHtml.hidden { () =>
                if (queryString.isEmpty)
                  JsCmds.Noop
                else
                  redoSearch(0, SearchQuery(queryString))
              })).apply(xml))
      } &
      "#results *" #> {
        val results = page
        if (results.isEmpty && !queryString.isEmpty) {
          ".entry *" #> ((xml: NodeSeq) => { entryHtml = xml; NodeSeq.Empty }) &
            ".no_results" #> { (xml: NodeSeq) =>
              { noResultsHtml = xml; (".query *" #> queryString.get).apply(xml) }
            } &
            ".index" #> ((xml: NodeSeq) => { welcomeHtml = xml; NodeSeq.Empty })
        } else if (results.isEmpty && queryString.isEmpty) {
          ".entry *" #> ((xml: NodeSeq) => { entryHtml = xml; NodeSeq.Empty }) &
            ".no_results" #> ((xml: NodeSeq) => { noResultsHtml = xml; NodeSeq.Empty }) &
            ".index" #> ((xml: NodeSeq) => { welcomeHtml = xml; xml })
        } else {
          ".entry" #> ((xml: NodeSeq) => { entryHtml = xml; xml }) &
            ".entry *" #> page.map { res: SearchResultEntry =>
              toCss(res)
            } &
            ".no_results" #> ((xml: NodeSeq) => { noResultsHtml = xml; (".no_results *" #> NodeSeq.Empty).apply(xml) })
        }
      } &
      ("#" + paginationBarId + " *") #> { (xml: NodeSeq) => { paginationHtml = xml; renderPagination(paginationHtml) } }
  }

  private def createC3SearchQuery(contentQuery: String) = {
    if (!contentQuery.isEmpty) contentQuery.trim.split(" ").map(str => { "" + str + "" }).mkString(" ")
    else contentQuery

  }

  private def search(query: String): List[SearchResultEntry] = {
    logger.debug("Query to C3: " + query)
    c3.search(query)
  }

  private def initSearchParams() {
    queryString.set(S.param("query").openOr(""))
  }
}

case class SearchQuery(value: String)

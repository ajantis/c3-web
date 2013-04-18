package org.aphreet.c3.snippet.search.snippet

import net.liftweb.util._
import Helpers._
import org.aphreet.c3.model._
import net.liftweb.http._
import net.liftweb.http.js.{JE, JsCmd, JsCmds}
import net.liftweb.http.js.jquery.JqJsCmds
import xml.{Unparsed, NodeSeq}
import org.aphreet.c3.lib.DependencyFactory._
import com.ifunsoftware.c3.access.C3System
import net.liftweb.util.CssSel
import java.text.SimpleDateFormat
import org.aphreet.c3.lib.metadata.Metadata
import org.aphreet.c3.util.C3Loggable
import com.ifunsoftware.c3.access.SearchResultEntry

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 */
class SearchForm extends PaginatorSnippet[SearchResultEntry] with C3Loggable{

  private val c3 = inject[C3System].open_!
  private val dateFormat = new SimpleDateFormat("MMM dd, yyyy")

  val selectedTagsContainerId = "selected_tags"
  val tagTemplate = <li class="tag"><a href="#" class="label btn-info name">Sample tag</a></li>
  var entryHtml = NodeSeq.Empty
  var noResultsHtml = NodeSeq.Empty
  var paginationHtml = NodeSeq.Empty

  val queryInputId = "s_query"
  val queryParam = "query"
  val paginationBarId = "pagination"

  object queryString extends SessionVar[String]("")
  object tags extends SessionVar[Set[String]](Set())
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
      <li><a href="#">{ns}</a></li>
    else {
      val (liClass, onClick) =
        if (first == newFirst)
          ("active", "#")
        else
          ("", SHtml.ajaxCall(JE.ValById(queryInputId), s => redoSearch(newFirst, SearchQuery(s, Set())))._2.toJsCmd)

      <li class={liClass}><a onclick={onClick}>{ns}</a></li>
    }

  /**
   * Returns a URL used to link to a page starting at the given record offset.
   */
  override def pageUrl(offset: Long): String =
    appendParams(super.pageUrl(offset), List(queryParam -> queryString))

  private def redoSearch(pageNum: Long, q: SearchQuery): JsCmd = {
    firstP.set(pageNum)

    queryString.set(q.value)
    tags.set(q.tags)

    results.set(search(createC3SearchQuery(queryString, tags)))

    val resultsHtml: NodeSeq = {
      if (results.isEmpty)
        (".query *" #> queryString.get).apply(noResultsHtml)
      else
        page.flatMap(entry => toCss(entry).apply(entryHtml))
    }


    JsCmds.SetHtml("results", resultsHtml) &
    JsCmds.SetHtml(paginationBarId, renderPagination(paginationHtml))
  }

  private def toCss(result: SearchResultEntry): CssSel = {
    val resource = c3.getResource(result.address, List("c3.ext.fs.path"))
    val c3Path = C3Path(result.path)
    val nodeName = resource.systemMetadata.getOrElse("c3.fs.nodename", "<Unknown>")
    val content = result.fragments.headOption.flatMap(_.strings.headOption.map(_.take(50)))
    //      val resourceName = c3Path.resourceType match {
    //        case MessagesType => "Message in group: " + c3Path.groupName
    //        case _ => c3Path.resourceName
    //      }
    val tags = resource.metadata.get(Metadata.TAGS_META).map(_.split(",").toList).getOrElse(Nil)

    ".result_header *" #> nodeName &
      ".result_header [href]" #> c3Path.resourceParentDir &
      ".result_link [href]" #> c3Path.resourceUri &
      ".result_link *" #> c3Path.resourceUri &
      ".result_content *" #> Unparsed(content.getOrElse("")) &
      ".result_date *" #> dateFormat.format(resource.date) &
      ".tag *" #> {
        ".label *" #> tags
      }
  }

  private def selectTag(tag: Tag, q: String): JsCmd = {
    val newSQuery = SearchQuery(q, tags.get + tag.name.is)

    JsCmds.Replace("tag_" + tag.id.is, NodeSeq.Empty) &
    JqJsCmds.AppendHtml(selectedTagsContainerId,
      <li id={"sel_tag_" + tag.id.is} class="label btn-info sel-tag">
        <span>{tag.name.is}</span>
        <a onclick={SHtml.ajaxCall(JE.ValById(queryInputId), s => unselectTag(tag, s))._2.cmd.toJsCmd}>
          <i class="icon-remove-sign icon-white"></i>
        </a>
      </li>
    ) & redoSearch(0, newSQuery)
  }

  private def unselectTag(tag: Tag, q: String): JsCmd = {
    val newSQuery = SearchQuery(q, tags.get - tag.name.is)

    JqJsCmds.AppendHtml("category_" + tag.category.get + "_tags", tagToCss(tag)(tagTemplate)) &
    JsCmds.Replace("sel_tag_" + tag.id.is, NodeSeq.Empty) &
    redoSearch(0, newSQuery)
  }

  private def tagToCss(tag: Tag) = {
    "li [id]" #> ("tag_" + tag.id.is) &
    ".name *" #> tag.name.is &
    ".name [onclick]" #> SHtml.ajaxCall(JE.ValById(queryInputId), s => selectTag(tag, s))
  }

  def miniSearch = {
    def process(query: String){
      if (!query.isEmpty){
        S.redirectTo("/search?query=" + urlEncode(query))
      }
      else S.notice("Empty search query")
    }
    "name=query [value]" #> S.param("query") &
    "name=query" #> SHtml.onSubmit(process _)
  }

  private def renderPagination(xml: NodeSeq) = paginate(xml)

  def render = {
    initSearchParams()

    ".category *" #> Category.findAll().map {
      category => {
        val tags = category.tags.toList
        "ul [id]" #> ("category_" + category.id.is + "_tags") &
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
         "type=submit" #> ( (xml: NodeSeq) =>
           xml ++ SHtml.hidden{ () =>
             if(queryString.isEmpty)
               JsCmds.Noop
             else
               redoSearch(0, SearchQuery(queryString, tags))
         })).apply(xml)
      )
    } &
    "#results *" #> {
      val results = page
      if (results.isEmpty){
        ".entry *" #> ((xml: NodeSeq) => { entryHtml = xml; NodeSeq.Empty }) &
        ".no_results" #> { (xml: NodeSeq) =>
          { noResultsHtml = xml; (".query *" #> queryString.get).apply(xml) }
        }
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

  private def createC3SearchQuery(contentQuery: String, tags: Iterable[String]) = {
    "content:\"" + contentQuery + "\"" +
    (if (!tags.isEmpty){
      " AND (" +
        tags.map { t => "(" + Metadata.TAGS_META + ":\"" + t + "\")" }.mkString(" AND ") +
      ")"
    } else "")
  }

  private def search(query: String): List[SearchResultEntry] = {
    logger.debug("Query to C3: " + query)
    val userGroupsIds = User.currentUser.map(_.groups.map(_.id.is)).openOr(Nil)
    val openGroups = Group.findOpenGroups.map(_.id.is)
    c3.search(query).filter{p =>
      if(!p.path.isEmpty){
        val groupId = C3Path(p.path).groupName.toLong
        userGroupsIds.contains(groupId) || openGroups.contains(groupId)
      }else{
        false
      }

    }
  }

  private def initSearchParams(){
    queryString.set(S.param("query").openOr(""))
    tags.set(Set())
    results.set(search(createC3SearchQuery(queryString, tags)))
  }
}

case class SearchQuery(value: String, tags: Set[String])

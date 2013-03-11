package org.aphreet.c3.snippet.search.snippet

import net.liftweb.util.Helpers._
import org.aphreet.c3.model.{Tag, Category}
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmd, jquery, JsCmds}
import jquery.JqJsCmds
import xml.NodeSeq
import net.liftweb.http.js.jquery.JqJsCmds._

/**
 * Copyright iFunSoftware 2013
 * @author Dmitry Ivanov
 */
class SearchForm {

  val selectedTagsContainerId = "selected_tags"
  val tagTemplate = <li class="tag"><a href="#" class="label btn-info name">Sample tag</a></li>

  def render = {

    def selectTag(tag: Tag): JsCmd = {
      JsCmds.Replace("tag_" + tag.id.is, NodeSeq.Empty) &
      JqJsCmds.AppendHtml(selectedTagsContainerId,
        <li id={"sel_tag_" + tag.id.is} class="label btn-info sel-tag">
          <span>{tag.name.is}</span>
          <a onclick={SHtml.ajaxInvoke(() => unselectTag(tag))._2.cmd.toJsCmd}>
            <i class="icon-remove-sign icon-white"></i>
          </a>
        </li>
      )
    }

    def unselectTag(tag: Tag): JsCmd = {
      JqJsCmds.AppendHtml("category_" + tag.category.get + "_tags", tagToCss(tag)(tagTemplate)) &
      JsCmds.Replace("sel_tag_" + tag.id.is, NodeSeq.Empty)
    }

    def tagToCss(tag: Tag) = {
      "li [id]" #> ("tag_" + tag.id.is) &
      ".name *" #> tag.name.is &
      ".name [onclick]" #> SHtml.ajaxInvoke(() => selectTag(tag))
    }

    ".category *" #> Category.findAll().map {
      category => {
        val tags = category.tags.toList
        "ul [id]" #> ("category_" + category.id.is + "_tags") &
        ".name *" #> category.name.is &
        ".tag" #> tags.map { tag =>
          tagToCss(tag)
        }
      }
    }
  }

}

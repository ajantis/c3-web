package org.aphreet.c3.snippet.categories.snippet

import net.liftweb.common.{Empty, Full}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.{S, SHtml}
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import org.aphreet.c3.model.{Category, Tag, User}
import org.aphreet.c3.util.helpers.AdminPageHelper

import scala.xml.NodeSeq

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 * @author Tkachev Alexey (mailto: imsiral1@mail.ru)
 */

class CategoryListPage extends AdminPageHelper {

  override lazy val activeLocId = "categories"

  def list = {
    val categories = Category.findAll()

    def categoryContents(cat: Category) = {
      val categoryFormId = "cat_" + cat.id.is

      def deleteCat(): JsCmd = {
        cat.delete_!
        JsCmds.Replace(categoryFormId.toString, NodeSeq.Empty)
      }

      val tags: List[Tag] = cat.tags.toList

      ".cat [id]" #> categoryFormId &
        ".close [onclick]" #> SHtml.ajaxInvoke(() => deleteCat()) &
        ".tag" #> tags.map {
          tag: Tag =>
            {
              val formId = "tag_" + tag.id.is
              def deleteTag(): JsCmd = {
                tag.delete_!
                JsCmds.Replace(formId, NodeSeq.Empty)
              }
              ".tag [id]" #> formId &
                ".close [onclick]" #> SHtml.ajaxInvoke(() => deleteTag()) &
                "span *" #> tag.name.is
            }
        } &
        ".muted *" #> cat.name &
        ".tagAddButton [id]" #> cat.id.is
    }
    ".tags_cont" #> categories.map { cat: Category => categoryContents(cat) } &
      "#new_tags" #> AddTag.addTags andThen
      "#edit_category" #> editCategory
  }

  def adm = {
    User.currentUser match {
      case Full(user) => {
        "#new_category" #> addCategory &
          "#edit_category" #> editCategory andThen
          "* *" #> ((x: NodeSeq) => x)
      }
      case _ => {
        ".add_cat" #> NodeSeq.Empty
      }
    }
  }
  def addCategory = {
    val category = Category.create
    def process() {
      category.validate match {
        case Nil => {
          category.save()
          S.notice("Category is added")
        }
        case xs =>
          xs.foreach(f => S.error(f.msg))
      }
    }
    "name=new_category_name" #> SHtml.onSubmit(category.name(_)) &
      "type=submit" #> SHtml.onSubmitUnit(process)
  }
  def editCategory = {

    var categoryName = ""
    var categoryNewName = ""

    def process() = {
      Category.find(By(Category.name, categoryName)) match {
        case Full(c) => {
          c.name(categoryNewName).validate match {
            case Nil => {
              c.save()
            }
            case xs =>
              S.notice("Category already exists or name is not valid")
          }
        }
        case Empty => S.error("Category is not found!")
      }
    }

    "name=old_name" #> SHtml.onSubmit(categoryName = _) &
      "name=category_new_name" #> SHtml.onSubmit(categoryNewName = _) &
      "type=submit" #> SHtml.onSubmitUnit(process)
  }
}


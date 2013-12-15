package org.aphreet.c3.snippet.categories.snippet

import net.liftweb.util.Helpers._
import org.aphreet.c3.model.{User, Tag, Category}
import xml.NodeSeq
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds.{OnLoad, Script}
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.{Empty, Full}
import net.liftweb.mapper.By

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 */

class CategoryListPage {
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
        ".close [onclick]" #> SHtml.ajaxInvoke(()=> deleteCat() ) &
        ".tag" #> tags.map {
          tag: Tag => {
            val formId = "tag_" + tag.id.is
            def deleteTag(): JsCmd = {
              tag.delete_!
              JsCmds.Replace(formId, NodeSeq.Empty)
            }
            ".tag [id]" #> formId &
              ".close [onclick]" #> SHtml.ajaxInvoke(()=> deleteTag())&
              "span *" #> tag.name.is
          }
        } &
        ".muted *" #> cat.name &
        ".tagAddButton [id]" #> cat.id.is
    }
    ".tags_cont" #> categories.map{ cat:Category =>  categoryContents(cat) } &
      "#new_tags" #> AddTag.addTags &
      "#edit_category" #> editCategory andThen
      "* *" #> ((x: NodeSeq) => x ++ Script(OnLoad(JsCmds.JsHideId("left-panel"))))
  }

  def adm = {
    User.currentUser match {
      case Full(user) => {
        "#new_category" #> addCategory andThen
          //"#edit_category" #> editCategory andThen
          "* *" #> ((x: NodeSeq) => x)
      }
      case _ =>{
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
    "name=new_category_name" #> SHtml.onSubmit(category.name(_))&
      "type=submit" #> SHtml.onSubmitUnit(process)
  }
  def editCategory = {

            var categoryNewName = ""

            def process() = {
             val category = Category.findAll(By(Category.name,categoryNewName))(0)
              category.validate match {
                case Nil => {
                  S.notice("Category already exists or name is not valid")
                }
                case xs =>
                  category.name(categoryNewName).save()
              }
            }

            "name=category_new_name" #> SHtml.onSubmit(categoryNewName =_)&
              "type=submit" #> SHtml.onSubmitUnit(process)

//    var categoryId = ""
//    var categoryNewName = ""
//
//    def process() = {
//      Category.find(categoryId) match {
//        case Full(c) => {
//          c.validate match {
//            case Nil => {
//              S.notice("Category already exists or name is not valid")
//            }
//            case xs =>
//              c.name(categoryNewName).save()
//          }
//        }
//        case Empty => S.error("Category is not found!")
//      }
//    }
//
//    "name=category_id" #> SHtml.onSubmit(categoryId = _)&
//      "name=category_new_name" #> SHtml.onSubmit(categoryNewName =_)&
//      "type=submit" #> SHtml.onSubmitUnit(process)
  }
}



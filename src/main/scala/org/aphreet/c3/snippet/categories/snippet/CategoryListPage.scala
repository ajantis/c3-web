package org.aphreet.c3.snippet.categories.snippet

import net.liftweb.util.Helpers._
import org.aphreet.c3.model.{User, Tag, Category}
import net.liftweb.mapper.{OprEnum, Cmp, By}
import xml.NodeSeq
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsCmds.{OnLoad, Script}
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full

class CategoryListPage {

  val categories = Category.findAll()
  def list = {

    var flag1 = 0
    var flag2 = 0
    val categories = Category.findAll()
    def categoryContents(cat: Category) = {
      flag2+=1
      val id = "tab"+flag2
      var id_span = 0
      val tagNames = Tag.findAll(By(Tag.category, cat))
      ".label_tags" #> tagNames.map{tg:Tag =>{
        id_span+=1
        "span *" #> tg.name &
        "span [id]" #> (id+"_"+id_span)
      }}&
      ".muted *" #> cat.name &
      ".muted [id]" #> id
    }
    ".well *" #> categories.map{ cat:Category =>  categoryContents(cat) } andThen
    "* *" #> ((x: NodeSeq) => x ++ Script(OnLoad(JsCmds.JsHideId("left-panel"))))

  }
  def adm = {
    User.currentUser match {
      case Full(user) => {
        "* *" #> ((x: NodeSeq) => x)
      }
      case _ =>{
        ".add_cat" #> NodeSeq.Empty
      }
    }
  }
  def render = {
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
    "name=categories" #> SHtml.onSubmit(category.name(_))&
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def del_tag = {
    var tagsName=""
    var catName=""
    def process(){

      // TODO: Tag.find(By(Tag.name, tagsName), By(Tag.category, 1)).foreach(_.delete_!)

      val cat = Category.find(By(Category.name,catName))
      cat.map{ ct:Category =>{
        Tag.find(By(Tag.name,tagsName),By(Tag.category,ct.id)).foreach(_.delete_!)
      }}
    }
    "name=tagName" #> SHtml.onSubmit(tagsName = _)&
    "name=categoryName" #> SHtml.onSubmit(catName = _)&
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

}


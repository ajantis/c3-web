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

      ".label_tags *" #> tagNames.map{tg:Tag =>{
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
    var categoryName = ""
    def process() {
      if (Category.find(Cmp(Category.name, OprEnum.Eql, Full(categoryName.toLowerCase), None, Full("LOWER"))).isEmpty){
        Category.create.name(categoryName).saveMe()
        S.notice("Category is added.")
      }
      else{
        S.error("This category already exists.")
      }
    }
    "name=categories" #> SHtml.onSubmit(categoryName = _)&
      "type=submit" #> SHtml.onSubmitUnit(process)
  }
  def del_tag = {
    var tagsName=""
    var catName=""
    def process(){
      Tag.find(By(Tag.name,tagsName)).foreach(_.delete_!)

    }
    "name=tagName" #> SHtml.onSubmit(tagsName = _)&
    "name=categoryName" #> SHtml.onSubmit(catName = _)&
    "type=submit" #> SHtml.onSubmitUnit(process)

  }

}


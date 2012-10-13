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
  def list = {
    val categories = Category.findAll()

    var flag1 = 0
    var flag2 = 0

    def tabs(cat: Category) = {
        flag1+=1
        val hrf = "#tab" + flag1
        "a *" #> cat.name.is &
        "a [href]" #> hrf
    }

    def categoryContents(cat: Category) = {
      flag2+=1
      val id = "tab"+flag2
      var id_span = 0
      val tagNames = Tag.findAll(By(Tag.category, cat))

      "span" #> tagNames.map{tg:Tag =>{
        id_span+=1
        "span *" #> tg.name &
        "span [id]" #> (id+"_"+id_span)
      }}&
      ".tab-pane [id]" #> id
    }

    ".tabs-left1" #> categories.map{ cat: Category => tabs(cat) } &
    ".tab-pane" #> categories.map{ cat:Category =>  categoryContents(cat) } andThen
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

}


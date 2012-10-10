package org.aphreet.c3.snippet.categories

import net.liftweb.util.Helpers._
import net.liftweb.util._
import org.aphreet.c3.model.{Tag, Category}
import net.liftweb.mapper.By
import xml.{Text, NodeSeq}
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsCmds.{OnLoad, Script}
import net.liftweb.http.{SHtml, S}

class CategoryListPage {

  def list = {
    val categories = Category.findAll()

    var flag1 = 0
    var flag2 = 0

    def tabs(cat: Category) = {
        flag1+=1
        val hrf = "#tab" + flag1

//        def setActive: CssSel = ".tabs-left1 [class+]" #> { (x: NodeSeq) => {
//          if (flag1==1)
//           Text("active")
//          else  Text("")
//        }}

        "a *" #> cat.name.is &
          "a [href]" #> hrf //&
//        setActive
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
      ".tab-pane [id]" #> id //andThen
//      "#tab1 [class+]" #>"active"

    }

    ".tabs-left1" #> categories.map{ cat: Category => tabs(cat) } &
    ".tab-pane" #> categories.map{ cat:Category =>  categoryContents(cat) } andThen
    "* *" #> ((x: NodeSeq) => x ++ Script(OnLoad(JsCmds.JsHideId("right-panel"))))

  }

  def search = {
    "* *" #> ((x: NodeSeq) => x ++ Script(OnLoad(JsCmds.JsShowId("categories_s"))))
  }

  val tagSeparator = ","

  def render = {
    var tags: Array[String] = Array()
    var query = ""

    def process(){
      println(tags)
      val request = "/search?tags=%s&query=%s".format(tags.map(urlEncode _).mkString(tagSeparator), urlEncode(query))
      if (!tags.isEmpty || !query.isEmpty){
        S.redirectTo(request)
      }
    }
    "name=tags" #> SHtml.onSubmit(v => tags = v.split(tagSeparator)) &
    "name=query" #> SHtml.onSubmit(query = _) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}


package org.aphreet.c3.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util._
import org.aphreet.c3.model.{Tag, Category}
import net.liftweb.mapper.By
import xml.{Text, NodeSeq}
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsCmds.{OnLoad, Script}

class CategoriesSnippet {

  def list = {
    val categories = Category.findAll()

    var flag1 = 0
    var flag2 = 0

    def tabs(cat: Category) = {
        flag1+=1
        val hrf = "#tab" + flag1

        def setActive: CssSel = ".tabs-left1 [class+]" #> { (x: NodeSeq) => {
          if (flag1==1)
           Text("active")
          else  Text("")
        }}

        "a *" #> cat.name.is &
          "a [href]" #> hrf &
        setActive
    }

    def categoryContents(cat: Category) = {
      flag2+=1
      val id = "tab"+flag2
      val tagNames = Tag.findAll(By(Tag.category, cat)).map(_.name.is)

      ".tab-pane [id]" #> id &
      "span *" #> tagNames andThen
      "#tab1 [class+]" #>"active"

    }

    ".tabs-left1 *" #> categories.map{ cat: Category => tabs(cat) } &
    ".tab-pane" #> categories.map{ cat:Category =>  categoryContents(cat) } andThen
    "* *" #> ((x: NodeSeq) => x ++ Script(OnLoad(JsCmds.JsHideId("right-panel"))))

  }
}

package org.aphreet.c3.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util._
import org.aphreet.c3.model.{Tag, Category}
import net.liftweb.mapper.By

class CategoriesSnippet {
  def list = {

    val categories = Category.findAll()

    "li *" #> categories.map{ cat:Category => {
      val tagNames = Tag.findAll(By(Tag.category, cat)).map(_.name.is)

      "span *" #> cat.name.is &
      "li *" #> tagNames
    }}
  }

}

package org.aphreet.c3.snippet.categories.snippet

import org.aphreet.c3.model.{ User, Category, Tag }
import net.liftweb.mapper.{ By, OprEnum, Cmp }
import net.liftweb.common.Full
import net.liftweb.http.{ SHtml, S }
import net.liftweb.util.BindHelpers._
import net.liftweb.util.Helpers
import xml.NodeSeq

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 */
object AddTag {

  def adm = {
    User.currentUser match {
      case Full(user) => {
        "* *" #> ((x: NodeSeq) => x)
      }
      case _ => {
        ".add_cat" #> NodeSeq.Empty
      }
    }
  }

  def addTags = {
    var categoryId = ""
    var tags = ""

    def process() = {
      val category = Category.find(categoryId)
      if (category.isEmpty)
        S.error("Category is not found!")
      else if (tags == "")
        None
      else {
        val tagsList = tags.split(",").map(_.trim)
        var successfully = true
        tagsList.map(tagAdd => {
          val tag = Tag.create
          tag.name(tagAdd)
          tag.category(category.open_!)
          tag.validate match {
            case Nil => {
              tag.save()
            }
            case xs => {
              xs.foreach(fe => S.error(fe.msg))
              successfully = false
            }
          }

        })
        if (successfully)
          S.notice("Tag is added.")

      }
    }
    "name=category_id" #> SHtml.onSubmit(categoryId = _) &
      "name=tags_add" #> SHtml.onSubmit(tags = _) &
      "type=submit" #> SHtml.onSubmitUnit(process)
  }

}

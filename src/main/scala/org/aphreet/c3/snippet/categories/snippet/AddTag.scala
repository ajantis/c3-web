package org.aphreet.c3.snippet.categories.snippet

import org.aphreet.c3.model.{User, Category, Tag}
import net.liftweb.mapper.{By, OprEnum, Cmp}
import net.liftweb.common.Full
import net.liftweb.http.{SHtml, S}
import net.liftweb.util.BindHelpers._
import net.liftweb.util.Helpers
import xml.NodeSeq

/**
 * @author Serjk
 * C3 Web team
 */
class AddTag {

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
    var tagName = ""
    var categoryName = ""

    def process() {

      val cat = Category.find(Cmp(Category.name, OprEnum.Eql, Full(categoryName.toLowerCase), None, Full("LOWER")))

      cat match {
        case Full(c) => {
          if (Tag.find(Cmp(Tag.name, OprEnum.Eql, Full(tagName.toLowerCase), None, Full("LOWER"))).isEmpty){
            Tag.create.name(tagName).category(c).saveMe()
            S.notice("This tag is added.")
          }
          else{
            S.error("This tag exists.")
          }
        }
        case _ => {
          S.error("Category is not found!")
        }
      }
    }
    "name=categoryName" #> SHtml.onSubmit(categoryName = _)&
    "name=TagName" #> SHtml.onSubmit(tagName = _)&
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

}

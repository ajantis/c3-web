package org.aphreet.c3.snippet.categories.snippet

import net.liftweb.http.{SHtml, S}
import org.aphreet.c3.model.{User, Category}
import net.liftweb.util.Helpers._
import net.liftweb.mapper.{Cmp, OprEnum}
import net.liftweb.common.Full

import xml.NodeSeq
/**
 * @author Serjk
 * C3 Web team
 */
class AddCategory{
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

package org.aphreet.c3 {
package model {

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import net.liftweb.http.SHtml
import xml.{NodeSeq, Text}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] {



  override def dbTableName = "users" // define the DB table name

  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email,
  locale, timezone, password, textArea)

  // comment this line out to require email validations
  override def skipEmailValidation = true

  override def screenWrap = Full(
    <lift:surround with="default" at="content">
        <div class="content-header">
        </div>

			  <div class="content" >
           <div class="usermenu">
            <lift:bind />
           </div>
        </div>
    </lift:surround>
  )

}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] with ManyToMany {

  thisuser =>

  def getSingleton = User // what's the "meta" server

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }

  object groups extends MappedManyToMany(UserGroup, UserGroup.user, UserGroup.group, Group) {

    def toForm() : NodeSeq = {
      if(!this.toList.isEmpty) {
        {<ul>{
          for(group <- this.toList) yield
          (<li>{Text(group.name.is).toSeq ++ SHtml.checkbox(true, (selected: Boolean) => if(! selected) UserGroup.find(By(UserGroup.group,group),By(UserGroup.user,thisuser)).open_!.delete_!).toSeq}</li>).flatten
         }</ul>}.flatten
      }
      else Text("No groups.")
    }
  }



}

}
}

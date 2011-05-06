package org.aphreet.c3 {
package model {

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import xml.{XML, NodeSeq, Text}
import net.liftweb.http.S._
import net.liftweb.http.{SessionVar, S, SHtml}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] {

  def currentSearchRequests: List[String] = User.currentUser.map(_.searchRequests.get).openOr(Nil)
  def addSearchRequest(req : String) = {
    User.currentUser match {
      case Full(user) => user.searchRequests.set(req :: user.searchRequests.get)
      case _ => {}
    }
  }

  // for stateful redirects on login (depends on where user wanted to go)
  override def logUserIn(who: User, postLogin: () => Nothing) : Nothing = {
    val currentRedirect : Box[String] = loginRedirect
    super.logUserIn(who, () => {
        loginRedirect(currentRedirect)
        postLogin()
    })
  }

  override def dbTableName = "users" // define the DB table name

  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email,
  locale, timezone, password, textArea)

  // comment this line out to require email validations
  override def skipEmailValidation = true


  //for some reason this doest not work =(
  //
  /*override lazy val password = new MyPassword(this){
    override def _toForm: Box[NodeSeq] = {
      S.fmapFunc({s: List[String] => this.setFromAny(s)}){funcName =>
        Full(<span>
            <input id={fieldId} type='password' name={funcName} value={is.toString} placeholder="Type password here"/>
            <input type='password' name={funcName} value={is.toString} placeholder="Type confirmation here"/>
        </span>)
      }
    }
  }
  */

  override def lostPasswordXhtml = {
    (<div>
      <h1>{S.??("enter.email")}</h1>
      <div class="user-form-content">
        <form method="post" action={S.uri}>
          <table>
            <tr><td class="user-form-key">{userNameFieldString}</td><td class="user-form-value"><user:email /></td></tr>
            <tr><td class="user-form-key">&nbsp;</td><td class="user-form-submit"><user:submit /></td></tr>
          </table>
        </form>
      </div>
    </div>)
  }

  override def loginXhtml = {
    (<div>
      <h1>{S.??("log.in")}</h1>
      <div class="user-form-content">
        <form method="post" action={S.uri}>
          <table>
            <tr>
              <td class="user-form-key">{userNameFieldString}</td>
              <td class="user-form-value"><user:email /></td>
            </tr>
            <tr>
              <td class="user-form-key">{S.??("password")}</td>
              <td class="user-form-value"><user:password /></td>
            </tr>
            <tr>
              <td class="user-form-key"><a href={lostPasswordPath.mkString("/", "/", "")}>{S.??("recover.password")}</a></td>
              <td class="user-form-submit"><user:submit /></td>
            </tr>
          </table>
        </form>
      </div>
    </div>)
  }

  override protected def localForm(user: TheUserType, ignorePassword: Boolean, fields: List[FieldPointerType]): NodeSeq = {
    for {
      pointer <- fields
      field <- computeFieldFromPointer(user, pointer).toList
      if field.show_? && (!ignorePassword || !pointer.isPasswordField_?)
      form <- field.toForm.toList
    } yield <tr><td class="user-form-key">{field.displayName}</td><td class="user-form-value">{form}</td></tr>
  }


  override def signupXhtml(user: TheUserType) = {
    (<div>
      <h1>{S.??("sign.up")}</h1>
      <div class="user-form-content">
        <form method="post" action={S.uri}>
          <table>
            {localForm(user, false, signupFields)}
            <tr>
              <td class="user-form-key"></td>
              <td class="user-form-submit"><user:submit/></td>
            </tr>
          </table></form></div>
    </div>)
  }

  override def editXhtml(user: TheUserType) = {
    (<div>
      <h1>{S.??("edit")}</h1>
      <div class="user-form-content">
        <form method="post" action={S.uri}>
          <table>
            {localForm(user, true, editFields)}
            <tr>
              <td class="user-form-key"></td>
              <td class="user-form-submit"><user:submit/></td>
            </tr>
          </table>
        </form>
      </div>
    </div>)
  }

  override def screenWrap = Full(
    <lift:surround with="default" at="content">

      <div class="user-form">
          <lift:bind />
      </div>

    </lift:surround>
  )
  
  override def edit = {
    super.edit ++
    <br/>
    <lift:Menu.item name="ChangePassword">
      Change password
    </lift:Menu.item>.toList
  } : NodeSeq

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

  def categories: List[Category] = Category.findAll(By(Category.user,this))

  object searchRequests extends SessionVar[List[String]](Nil)

}

}
}

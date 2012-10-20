package org.aphreet.c3.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S._
import net.liftweb.http.{SessionVar, S, SHtml}
import net.liftweb.sitemap.Loc.LocGroup
import net.liftweb.util.BindHelpers._
import xml.{Text, NodeSeq, Elem}
import net.liftweb.http.js.JsCmds.FocusOnLoad

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User]{

  override def loginMenuLocParams = LocGroup("loginLogoutMenu") :: super.loginMenuLocParams
  override def createUserMenuLocParams = LocGroup("loginLogoutMenu") :: super.createUserMenuLocParams

  def currentSearchRequests: List[String] = User.currentUser.map(_.searchRequests.get).openOr(Nil)

  def addSearchRequest(req : String) = {
    User.currentUser match {
      case Full(user) => {
        if( req !="" )user.searchRequests.set(req :: user.searchRequests.get.filter(_ != req))
      }
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
    (<div class="forgot_password_form">
        <form action={S.uri} method="POST" class="form">
          <fieldset>
            <legend>Forgot Password</legend>
            <label for="email_field">
              Email<br/>
                <user:email />
            </label>
            <label>
                <user:submit />
            </label>

          </fieldset>
        </form>
      </div>)
  }

  override def loginXhtml = {
    (
      <div class="form-holder login-form well">
        <form action={S.uri} method="POST" class="form">
          <fieldset>
            <legend>Existing Users Login</legend>

            <label for="username">Email</label>
            <div class="div_text">
              <div class="input-prepend"><span class="add-on"><i class="icon-user"></i></span><user:email/></div>
            </div>

            <label for="password">Password</label>
            <div class="div_text">
              <div class="input-prepend"><span class="add-on"><i class="icon-lock"></i></span><user:password /></div>
            </div>

            <div class="button_div"><input name="rememberme" type="checkbox" id="rememberme" value="forever" />&nbsp;Remember me&nbsp;&nbsp;<user:submit/></div>

            <div class="clear"></div>
            <div align="right">Forgot password?&nbsp;<a href={this.lostPasswordMenuLoc.open_!.loc.createDefaultLink.get}>Click here to reset</a></div>
            <div align="right">New User?&nbsp;<a href={this.createUserMenuLoc.open_!.loc.createDefaultLink.get}>Click here to register</a></div>
            <div class="clear"></div>
          </fieldset>
        </form>
     </div>
    )
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

  override def login = {
    if (S.post_?) {
      S.param("username").
        flatMap(username => findUserByUserName(username)) match {
        case Full(user) if user.validated_? &&
          user.testPassword(S.param("password")) => {
          logUserIn(user, () => {
            S.notice(S.??("logged.in"))

            val redir = loginRedirect.is match {
              case Full(url) =>
                loginRedirect(Empty)
                url
              case _ =>
                homePage
            }
            S.redirectTo(redir)
          })
        }

        case Full(user) if !user.validated_? =>
          S.error(S.??("account.validation.error"))

        case _ => S.error(S.??("invalid.credentials"))
      }
    }

    bind("user", loginXhtml,
      "email" -> (FocusOnLoad(<input name="username" type="text" id="log inputIcon" value="" class="username span2" />)),
      "password" -> (<input name="password" type="password" id="pwd inputIcon" class="password span2" />),
      "submit" -> (<input type="submit" name="Submit" value="Login" class="btn btn-primary" />))
  }

}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] with ManyToMany   {


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


  object email extends MappedString(this, 128){
    def addClassCss: ElemSelector  =
      "input [class+]" #> "span3" &
      "input [placeholder]" #> S.?("email.placeholder")
    override def toForm:Box[Elem] =  super.toForm.map(e => addClassCss(NodeSeq.fromSeq(e.toSeq)))
  }

  object searchRequests extends SessionVar[List[String]] ( "scala" :: "java" :: "performance" :: "c3" :: Nil )

  def createLink: NodeSeq = Text("/users/" + id.is)
}
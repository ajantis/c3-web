package org.aphreet.c3.model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http.{SessionVar, S, SHtml}
import xml._
import net.liftweb.http.js.JsCmds.FocusOnLoad
import net.liftweb.util.Helpers._
import net.liftweb.http.S._
import net.liftweb.sitemap.Loc.LocGroup
import xml.Text
import net.liftweb.common.Full

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

  override def lostPassword = {


      bind("user", lostPasswordXhtml,
        "email" -> SHtml.text("", sendPasswordReset _, ("placeholder" -> S.?("email.placeholder")),("id"->"log inputIcon"),("class"->"username span2")),
        "submit" -> lostPasswordSubmitButton(S.?("repeat.password")))
  }

  override def lostPasswordXhtml = {
    (<div class="forgot_password_form form-holder login-form well">
        <form action={S.uri} method="POST" class="form">
          <fieldset>
            <legend>{S.?("lost_password.legend")}</legend>
            <label for="username">Email</label>
            <div class="div_text">
              <div class="input-prepend"><span class="add-on"><i class="icon-user"></i></span><user:email/></div>
            </div>

            <div class="button_div">
              <user:submit />
            </div>

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
    } yield <div class="input-prepend"><label for="password">{field.displayName}</label>{form}</div>
  }

  override def signupFields: List[FieldPointerType] = List(firstName,lastName,email, password)
  override def editFields: List[FieldPointerType] = List(firstName,lastName, email)

  override def signupXhtml(user: TheUserType) = {
    (<div class="form-holder signup-form login-form well">
      <form action={S.uri} method="POST" class="form">
        <fieldset>
          <legend>{S.?("signup.legend")}</legend>
            {localForm(user, false, signupFields)}

          <div class="button_div">
            <user:submit/>
          </div>
        </fieldset>
      </form>
    </div>)
  }

  override def editXhtml(user: TheUserType) = {
    ( <div class="form-holder edit-form login-form well">
        <form action={S.uri} method="POST" class="form">
          <fieldset>
            <legend>{S.?("edit.legend")}</legend>
            <div class="div_text">
              {localForm(user, true, editFields)}
            </div>
            <div class="button_div">
            <user:submit/>
              </div>
          </fieldset>
        </form>
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

  override def signup = {
    val theUser: TheUserType = mutateUserOnSignup(createNewUserInstance())
    val theName = signUpPath.mkString("")

    def testSignup() {
      validateSignup(theUser) match {
        case Nil =>
          actionsAfterSignup(theUser, () => S.redirectTo(homePage))

        case xs => S.error(xs) ; signupFunc(Full(innerSignup _))
      }
    }

    def innerSignup = bind("user",
      signupXhtml(theUser),
      "submit" -> signupSubmitButton(S.?("signup.button"), testSignup _))
    innerSignup
  }
  override def standardSubmitButton(name: String,  func: () => Any = () => {}) = {
    SHtml.submit(name, func,("class","btn btn-primary"))
  }


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
class User extends MegaProtoUser[User] with ManyToMany {
  thisuser =>

  def getSingleton = User // what's the "meta" server

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }

  object groups extends MappedManyToMany(UserGroup, UserGroup.user, UserGroup.group, Group){
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

      // this is just a prototype change
  override lazy val email: MappedEmail[T] = new MyEmail(this, 128){

    override def _toForm: Box[Elem] =
      fmapFunc({s: List[String] => this.setFromAny(s)}){name =>
        Full(appendFieldId(<div class="icon-envelope"><span class="add-on"><i class="icon-user"></i></span><input type={formInputType}
                                name={name}
                                class="span2"
                                placeholder ={S.?("email.placeholder")}
                                value={is match {case null => "" case s => s.toString}}/></div>))}

  }

  override lazy val firstName: MappedString[T] = new MyFirstName(this, 32){
    override def _toForm: Box[Elem] =
      fmapFunc({s: List[String] => this.setFromAny(s)}){name =>
        Full(appendFieldId(<div class="input-prepend"><span class="add-on"><i class="icon-user"></i></span><input type={formInputType} maxlength={maxLen.toString}
                                  name={name}
                                  class="username span2"
                                  placeholder ={S.?("firstName.placeholder")}
                                  value={is match {case null => "" case s => s.toString}}/></div>))}
  }
  override lazy val lastName: MappedString[T] = new MyLastName(this, 32) {
    override def _toForm: Box[Elem] =
      fmapFunc({s: List[String] => this.setFromAny(s)}){name =>
        Full(appendFieldId(<div class="input-prepend"><span class="add-on"><i class="icon-user"></i></span><input type={formInputType} maxlength={maxLen.toString}
                                  name={name}
                                  class="username span2"
                                  placeholder ={S.?("lastName.placeholder")}
                                  value={is match {case null => "" case s => s.toString}}/></div>))}
  }
  override lazy val password: MappedPassword[T] = new MyPassword(this){

    override def _toForm: Box[NodeSeq] = {
      S.fmapFunc({s: List[String] => this.setFromAny(s)}){funcName =>
        Full(<span>{appendFieldId(<div class="input-prepend"><span class="add-on"><i class="icon-lock"></i></span><input type={formInputType}
                                         name={funcName}
                                         class="password span2"
                                         value={is.toString}/></div>)}
          <label for="password">{S.?("repeat.password")}</label>
          <div class="icon-refresh"><span class="add-on"><i class="icon-lock"></i></span><input
          type={formInputType}
          name={funcName}
          class="password span2"
          value={is.toString}/></div></span>)
      }
    }
  }

  object searchRequests extends SessionVar[List[String]] ( "scala" :: "java" :: "performance" :: "c3" :: Nil )

  def createLink: NodeSeq = Text("/users/" + id.is)
}
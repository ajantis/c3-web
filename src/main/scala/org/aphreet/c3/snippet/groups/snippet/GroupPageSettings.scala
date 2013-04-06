package org.aphreet.c3.snippet.groups.snippet

import org.aphreet.c3.snippet.groups.{GroupPageData, AbstractGroupPageLoc}
import org.aphreet.c3.loc.SuffixLoc
import org.aphreet.c3.model.{User, Group}
import net.liftweb.common.Box
import net.liftweb.sitemap.Loc.Link
import org.aphreet.c3.lib.DependencyFactory._
import com.ifunsoftware.c3.access.C3System
import org.aphreet.c3.service.groups.GroupService
import net.liftweb.util.BindHelpers._
import xml.NodeSeq
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.S
import net.liftmodules.widgets.autocomplete.AutoComplete
import net.liftweb.http.js.JsCmds._Noop

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */

object GroupPageSettings extends AbstractGroupPageLoc[GroupPageData] with SuffixLoc[Group, GroupPageData] {
  override val name = "Settings"
  override val pathPrefix = "groups" :: Nil
  override val pathSuffix = "settings" ::  Nil
  override def getItem(id: String) = Group.find(id)
  override def wrapItem(groupBox: Box[Group]) = groupBox.map(new GroupPageData(_))

  override def link = {
    new Link[GroupPageData](pathPrefix ++ pathSuffix){
      override def pathList(value: GroupPageData): List[String] = pathPrefix ::: value.group.id.is.toString :: Nil ::: pathSuffix
    }
  }

}
class GroupPageSettings (data: GroupPageData) extends GroupPageHelpers{
  lazy val c3 = inject[C3System].open_!
  lazy val groupService = inject[GroupService].open_!

  override lazy val group = data.group
  override lazy val activeLocId = "settings"
  val members = group.users.all

  def owner = {
    ".GroupOwner *" #> group.owner.obj.map(_.shortName).openOr("N/A")&
      ".GroupOwner [href]" #> group.owner.obj.map(_.createLink)

  }
  def listUserAdd = {
    var users = User.findAll().filter(_.id.is != User.currentUser.open_!.id.is)
    members.map(user =>{
      users = users.filter(_.id.is != user.id.is)
    })
    if(users.isEmpty || User.currentUser.open_!.email.is != group.owner.obj.map(_.email).open_!.is){
      ".btn_add_user" #> NodeSeq.Empty
    }

  }
  def listUser = {
    if (User.currentUser.open_!.email.is == group.owner.obj.map(_.email).open_!.is){
      ".ListGroupUser" #> members.map(user =>{
        if (user.email.is != group.owner.obj.map(_.email).open_!.is){
          def deleteUser():JsCmd = {
            if(groupService.removeUserFromGroup(group,user)){
              JsCmds.Replace(user.id.is.toString, NodeSeq.Empty)
            } else JsCmds.Alert("User is not removed! Please check logs for details")
          }
          ".ListGroupUser [id]" #> user.id.is &
            ".ListGroupUser *" #>
              ((n: NodeSeq) => SHtml.ajaxForm(
                (".first_name *" #> user.firstName.is &
                  ".last_name *" #> user.lastName.is &
                  ".email *" #> user.email.is andThen
                  "* *" #> SHtml.memoize(f => f ++ SHtml.hidden(deleteUser _))).apply(n)
              ))
        }else{
          ".first_name *" #> user.firstName.is &
            ".last_name *" #> user.lastName.is &
            ".email *" #> user.email.is &
            ".deluser *" #> NodeSeq.Empty
        }

      })
    }else{
      ".ListGroupUser *" #> members.map(user =>{
        ".first_name *" #> user.firstName.is &
          ".last_name *" #> user.lastName.is &
          ".email *" #> user.email.is &
          ".deluser *" #> NodeSeq.Empty
      })

    }
  }

  def addUser = {
    var users = User.findAll().filter(_.id.is != User.currentUser.open_!.id.is)
    members.map(user =>{
      users = users.filter(_.id.is != user.id.is)
    })
    val data:List[String] = users.map(user=> user.email.is)
    ".list_users" #>  AutoComplete("", (current,limit) =>
      data.filter(_.toLowerCase.startsWith(current.toLowerCase)),
      value => addUserToGroup(value))
  }
  protected def addUserToGroup(userEmails:String) = {

    val members = User.findByEmail(userEmails)
    val (added, notAdded) = groupService.addUsersToGroup(group,members).partition(_.isDefined)

    if(!added.isEmpty)
      S.notice(userEmails +" is added to group " + group.name.is)
    if(!notAdded.isEmpty)
    // normally shouldn't happen
      S.error(userEmails +" is not added to group: " + group.name.is)
  }
  var param =  group.isOpen.is
  def publicSettings = {
    def saveCheckbox(b:Boolean):JsCmd = {
      group.isOpen(b).saveMe()
      param = b
      JsCmds.Noop
    }
    ".checkbox_public" #> SHtml.ajaxCheckbox(param,saveCheckbox(_))andThen
    ":checkbox [class+]" #> "floatLeft checkbox_public"

  }
}

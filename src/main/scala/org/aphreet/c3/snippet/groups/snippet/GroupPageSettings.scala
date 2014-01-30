package org.aphreet.c3.snippet.groups.snippet

import org.aphreet.c3.snippet.groups.{GroupPageData, AbstractGroupPageLoc}
import org.aphreet.c3.loc.SuffixLoc
import org.aphreet.c3.model.{UserGroup, User, Group}
import net.liftweb.common.Box
import net.liftweb.sitemap.Loc.Link
import org.aphreet.c3.lib.DependencyFactory._
import com.ifunsoftware.c3.access.{StringMetadataValue, MetadataUpdate, C3System}
import org.aphreet.c3.service.groups.GroupService
import net.liftweb.util.BindHelpers._
import xml.NodeSeq
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.S
import net.liftmodules.widgets.autocomplete.AutoComplete
import net.liftweb.http.js.JsCmds.{Function, Script, _Noop}
import org.aphreet.c3.snippet.LiftMessages
import net.liftweb.http.js.JE.JsVar
import com.ifunsoftware.c3.access.fs.C3FileSystemNode
import org.aphreet.c3.lib.metadata.Metadata._
import net.liftweb.http.js.JE.JsVar
import net.liftweb.mapper.By


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
  val members = UserGroup.findAll(By(UserGroup.group,group))


  val approvedMembers = members.filter(_.isApproved)
    .map(_.user.obj.openOrThrowException("Error open user"))

  val otherMembers = members.filter(_.isApproved!=true)
    .map(_.user.obj.openOrThrowException("Error open user"))

  def owner = {
    ".GroupOwner *" #> group.owner.obj.map(_.shortName).openOr("N/A")&
      ".GroupOwner [href]" #> group.owner.obj.map(_.createLink)

  }

  def listUserAdd = {
    var users = User.findAll().filter(_.id.is != User.currentUserUnsafe.id.is)
    approvedMembers.map(user =>{
      users = users.filter(_.id.is != user.id.is)
    })
    otherMembers.map(user =>{
      users = users.filter(_.id.is != user.id.is)
    })
  }

  def listUser = {
    ".ListGroupUser" #> approvedMembers.map(user =>{

      def deleteUser():JsCmd = {
        val currentUser = User.currentUserUnsafe
        val ownerGroup = group.owner.obj.openOrThrowException("Group haven't owner")
        user match{
          case usr if usr == currentUser => LiftMessages.ajaxError(S.?("remove.themselves"))
          case usr if usr == ownerGroup => LiftMessages.ajaxError(S.?("remove.owner"))
          case _ => if(groupService.removeUserFromGroup(group,user)){
            JsCmds.Replace(user.id.is.toString, NodeSeq.Empty)
          } else JsCmds.Alert("User is not removed! Please check logs for details")
        }

      }
      ".ListGroupUser [id]" #> user.id.is &
        ".first_name *" #> user.firstName.is &
        ".last_name *" #> user.lastName.is &
        ".email *" #> user.email.is  &
        ".full_name *" #> user.shortName &
        ".delete_member " #> SHtml.ajaxInvoke(()=>deleteUser())
    })
  }

  def listShortUser = {
    ".ListShortGroupUser" #> approvedMembers.map(user =>{
      ".email *" #> user.email &
        ".short_name [href]" #> user.createLink &
        ".short_name *" #> user.shortName
    })
  }
  //[TODO] need make immutable
  def addUser() = {
    var users = User.findAll().filter(_.id.is != User.currentUserUnsafe.id.is)
    approvedMembers.map(user =>{
      users = users.filter(_.id.is != user.id.is)
    })
    otherMembers.map(user =>{
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

  def publicSettings = {

    def saveCheckbox(b:Boolean):JsCmd = {
      group.isOpen(b).saveMe()
      JsCmds.Noop
    }

    def updateDescription(node:C3FileSystemNode, descr:String): JsCmd = {
      val meta = Map(DESCRIPTION_META -> StringMetadataValue(descr))
      node.update(MetadataUpdate(meta))
      JsCmds.Noop // bootstrap-editable will update text value on page by itself
    }

    ".checkbox_public" #> SHtml.ajaxCheckbox(group.isOpen.is,saveCheckbox) &
      ".name_group_settings *" #> group.name.is &
      ".description_box *"#> group.getDescription &
      ".description_submit_func *" #> {
        Script(
          Function("updateDescriptionCallback", List("description"),
            SHtml.ajaxCall(
              JsVar("description"),
              (str: String) => updateDescription(group.getGroupC3.openOrThrowException("File not found"), str)
            )._2.cmd
          )
        )
      }
  }

  def listUserApprove = {
    ".ListGroupUser" #> otherMembers.map{user:User =>

      def approveUser():JsCmd = {
        val (added, notAdded) = groupService.approveUsersToGroup(group,Iterable(user)).partition(_.isDefined)
        if(!added.isEmpty) {
          LiftMessages.ajaxNotice(user.niceName +" is approved to group " + group.name.is)&
          JsCmds.Replace(user.id.is.toString, NodeSeq.Empty)
        }
        if(!notAdded.isEmpty){
          // normally shouldn't happen
          LiftMessages.ajaxError(user.niceName +" is not approved to group: " + group.name.is)
        }
      }

      ".ListGroupUser [id]" #> user.id.is &
        ".first_name *" #> user.firstName.is &
        ".last_name *" #> user.lastName.is &
        ".email *" #> user.email.is  &
        ".full_name *" #> user.shortName &
        ".approve_member [onclick]" #> SHtml.ajaxInvoke(()=>approveUser())
    }
  }
}

package org.aphreet.c3.snippet.groups.snippet

import com.ifunsoftware.c3.access.C3System
import net.liftweb.common.{Empty, Logger}
import xml.NodeSeq
import org.aphreet.c3.model._
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.{JsCmds, JsCmd}
import org.aphreet.c3.service.groups.GroupService
import net.liftweb.util.Helpers._
import org.aphreet.c3.lib.DependencyFactory
import net.liftweb.util.CssSel
import net.liftweb.mapper.By
import net.liftweb.common.Full
import org.aphreet.c3.acl.groups.{UserStatusGroup, GroupsAccess}
import org.aphreet.c3.snippet.LiftMessages

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
class GroupListPage extends GroupsAccess{

  lazy val c3 = DependencyFactory.inject[C3System].open_!

  lazy val groupService = DependencyFactory.inject[GroupService].open_!

  lazy val logger = Logger(classOf[GroupListPage])

  lazy val lockGroupIcon = "glyphicons_203_lock.png"

  lazy val openGroupIcon = "glyphicons_043_group.png"

  def list = {

    val groupList =  User.currentUser match {
      case Full(u) =>   if(u.superUser.is) Group.findAll().toList
      else u.groups.toList ::: Group.findAll(By(Group.isOpen,true))

      case Empty => Group.findAll(By(Group.isOpen,true))
    }

    (User.currentUser match {
      case Full(u)=> "#add_group" #> addGroup()
      case Empty =>
        ".btn_add_user" #> NodeSeq.Empty &
          "#add_group" #> NodeSeq.Empty
    })&
      ".container_groups" #> groupList.distinct.sortBy(_.name.is).filter(_.isApproved).map {
        group: Group =>

      val groupIcon = if(group.isOpen.is) openGroupIcon else lockGroupIcon

      def infoGroup(picName:String):CssSel = {
        val groupLink = s"/groups/${group.id}/files/"
        val groupTags = group.getTags
        ".tags_group" #> groupTags.map((tag: String) => {
          ".tags_group *" #> tag
        }) &
          ".inf_left_groups [src]"#> ("/images/"+picName)&
          "a *" #> group.name.is &
          "a [href]" #> groupLink &
          ".description_group *"#> group.getDescription &
          ".owner_group  *" #> group.owner.obj.map(_.shortName).openOr("N/A")
      }

      def adminGroup():CssSel = {
        val settingsLink = s"/groups/${group.id}/settings"
        userGroup()&
          ".cog [onClick]" #> SHtml.ajaxInvoke(()=> JsCmds.RedirectTo(settingsLink))
      }

      def userGroup():CssSel = {
        ".plus" #> NodeSeq.Empty &
          ".event *" #> "3" //[TODO] add count events & redirect to event page
      }

      def lockGroup():CssSel = {
        ".cog" #> NodeSeq.Empty&
          ".event" #> NodeSeq.Empty
      }

      def sendRequest():JsCmd = {
        val user = User.currentUserUnsafe
        groupService.addUsersToApproveListGroup(group,Iterable(user))
        LiftMessages.ajaxNotice(user.niceName +" is add to approve list of group " + group.name.is)&
          JsCmds.Replace(group.id.is.toString, NodeSeq.Empty)
      }

      (User.currentUser match {
        case Full(u) => checkAccess(u,group) match {
          case UserStatusGroup.Admin =>
            adminGroup()

          case UserStatusGroup.Owner   =>
            adminGroup()

          case UserStatusGroup.Member  =>
            userGroup()&
              ".cog" #> NodeSeq.Empty

          case UserStatusGroup.Request =>
            lockGroup()&
              ".plus" #> NodeSeq.Empty

          case UserStatusGroup.Other   =>
            lockGroup()&
              ".plus [id]" #> group.id.is&
              ".plus [onclick]" #> SHtml.ajaxInvoke(()=> sendRequest())
        }
        case Empty =>
          lockGroup()&
            ".plus [onclick]" #> SHtml.ajaxInvoke(()=>  LiftMessages.ajaxNotice(S.?("response.login")))
      })&
        infoGroup(groupIcon)
    }
  }

  def addGroup():CssSel = {
    var newGroup = Group.create
    var public = ""

    def saveMe(){
      newGroup.validate match {
        case Nil =>

          newGroup = newGroup.owner(User.currentUserUnsafe)

          if (public != "false") newGroup.isOpen(true)
          if(newGroup.save) S.notice(S.?("approve.list.group") + newGroup.name)
          else S.warning(newGroup.name + " isn't 7added")

          //correct ID if group already exists
          var groupExists = true
          while (groupExists)
          {
            //check group existence in C3
            newGroup.getGroupC3 match {
              case Full(groupFileNode) =>
                groupExists = true;

                val savedGroup = newGroup;

                //delete group with existing id
                newGroup.delete_!

                // try again (id will incremented automaticly)
                newGroup = Group.create
                newGroup.description(savedGroup.description)
                newGroup.tags(savedGroup.description)
                newGroup.name(savedGroup.name)
                newGroup = newGroup.owner(User.currentUserUnsafe)

                if (public != "false") newGroup.isOpen(true)
                newGroup.isApproved(false)
                newGroup.save
              case _                   =>
                groupExists = false;
            }
          }
        case xs =>
          xs.foreach(f => S.error(f.msg))
      }
    }
    "name=name" #> SHtml.onSubmit(newGroup.name(_))&
      "name=description" #> SHtml.onSubmit(newGroup.description(_))&
      "name=public" #> SHtml.onSubmit(public = _) &
      "name=tags_edit" #> SHtml.onSubmit(newGroup.tags(_)) &
      "type=submit" #> SHtml.onSubmitUnit(saveMe)
  }


}

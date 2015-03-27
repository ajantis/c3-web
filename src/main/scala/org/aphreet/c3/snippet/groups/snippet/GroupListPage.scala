package org.aphreet.c3.snippet.groups.snippet

import com.ifunsoftware.c3.access.C3System
import net.liftweb.common.{Empty, Logger}
import org.aphreet.c3.lib.DependencyFactory._
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
import com.ibm.icu.text.Transliterator
import scala.util.matching
import java.lang.String;

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
class GroupListPage extends GroupsAccess{

  lazy val c3 = DependencyFactory.inject[C3System].open_!

  lazy val groupService = DependencyFactory.inject[GroupService].open_!

  lazy val logger = Logger(classOf[GroupListPage])

  lazy val lockGroupIcon = "glyphicons_203_lock.png"

  lazy val openGroupIcon = "glyphicons_043_group.png"

  val AllParameter: String = "all"
  val MyOwnGroupsParameter: String = "myOwnGroups"
  val MyGroupsParameter: String = "myGroups"
  val activeClass = "active"

  def list = {

    val tab = S.param("tab") openOr MyOwnGroupsParameter
    val groupList = User.currentUser match {
      case Full(u) =>
        tab match {
          case MyOwnGroupsParameter => Group.findAll(By(Group.owner, u))
          case MyGroupsParameter => u.groups.toList
          case _ => if (u.superUser.is) Group.findAll().toList
                    else u.groups.toList ::: Group.findAll(By(Group.isOpen, true))
        }
      case Empty => Group.findAll(By(Group.isOpen, true))
    }

      "#header *" #> (tab match {
        case MyGroupsParameter => "My Groups"
        case MyOwnGroupsParameter => "My Own Groups"
        case _ => "All Groups"
      }) &
      (User.currentUser match {
        case Full(u)=> "#add_group" #> addGroup()
        case Empty =>
          ".btn_add_user" #> NodeSeq.Empty &
            "#add_group" #> NodeSeq.Empty
      }) &
      ".container_groups" #> groupList.distinct.sortBy(_.name.is).filter(_.isApproved).map {
        group: Group =>

      val groupIcon = if(group.isOpen.is) openGroupIcon else lockGroupIcon

      def infoGroup(picName:String):CssSel = {

        val groupLink = s"/groups/${group.getId}/files/"
        val groupTags = group.getTags
        ".tags_group" #> groupTags.map((tag: String) => {
          ".label *" #> tag
        }) &
          ".inf_left_groups [src]"#> ("/images/"+picName)&
          "a *" #> group.name.is &
          "a [href]" #> groupLink &
          ".description_group *"#> group.getDescription &
          ".owner_group  *" #> group.owner.obj.map(_.shortName).openOr("N/A")
      }
      def adminGroup():CssSel = {
        val settingsLink = s"/groups/${group.getId}/settings"
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
          JsCmds.Replace(group.getId, NodeSeq.Empty)
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
              ".plus [id]" #> group.getId &
              ".plus [onclick]" #> SHtml.ajaxInvoke(()=> sendRequest())
        }
        case Empty =>
          lockGroup()&
            ".plus [onclick]" #> SHtml.ajaxInvoke(()=>  LiftMessages.ajaxNotice(S.?("response.login")))
      })&
        infoGroup(groupIcon)
    }
  }

  def newGroup = User.currentUser match {
    case Full(u) => "#add_group" #> addGroup()
    case Empty =>
      ".btn_add_user" #> NodeSeq.Empty &
        "#add_group" #> NodeSeq.Empty
  }

  def tabs = {
    val tab = S.param("tab") openOr MyGroupsParameter
    tab match {
      case MyGroupsParameter => "#myGroups [class+]" #> activeClass
      case MyOwnGroupsParameter => "#myOwnGroups [class+]" #> activeClass
      case _ => "#all [class+]" #> activeClass
    }
  }

  def addGroup():CssSel = {
    var newGroup = Group.create
    var public = ""

    def saveMe(){
      newGroup.validate match {
        case Nil =>
          val groupUID = GetGroupUID(newGroup.name)
          newGroup.uid(groupUID)
          newGroup = newGroup.owner(User.currentUserUnsafe)

          if (public != "false") newGroup.isOpen(true)
          if(newGroup.save) S.notice(S.?("approve.list.group") + newGroup.name)
          else S.warning(newGroup.name + " isn't added")

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

  def GetGroupUID(groupName:String): String = {
    val cyrillicToLatin = Transliterator.getInstance("Any-Latin; NFD; [^\\p{Alnum}] Remove")
    val latinGroupName = cyrillicToLatin.transliterate(groupName).replace(" ", "_")

    var uniqueGroupName = latinGroupName.replaceAll("[^A-Za-z0-9_-]","")

    var additionalId = 1;
    val c3 = inject[C3System].open_!

    while (c3.getFile("/").asDirectory.children().exists(c=>c.name == uniqueGroupName)) {
      additionalId=additionalId + 1
      uniqueGroupName = uniqueGroupName+"_"+additionalId.toString
    }
    return uniqueGroupName
  }
}

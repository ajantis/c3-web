package org.aphreet.c3.snippet.groups.snippet

import com.ifunsoftware.c3.access.C3System
import net.liftweb.common.{Failure, Full, Empty, Logger}
import xml.{Text, NodeSeq}
import org.aphreet.c3.model._
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.http.js.{JsCmds, JsCmd}
import org.aphreet.c3.service.groups.GroupService
import net.liftweb.util.Helpers._
import net.liftweb.util._
import org.aphreet.c3.lib.DependencyFactory
import net.liftweb.util.CssSel
import net.liftweb.mapper.By
import net.liftweb.common.Full

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
class GroupListPage {

  lazy val c3 = DependencyFactory.inject[C3System].open_!

  lazy val groupService = DependencyFactory.inject[GroupService].open_!

  lazy val logger = Logger(classOf[GroupListPage])

  def list = {

    val groupList =  User.currentUser match {
      case Full(u) =>   if(u.superUser.is) Group.findAll().toList
        else u.groups.toList ::: Group.findAll(By(Group.isOpen,true))

      case Empty => Group.findAll(By(Group.isOpen,true))
    }

    ".container_groups" #> groupList.toSet.filter(_.isApprove==true).map{ group:Group => {

      val picName = if(!group.isOpen) "glyphicons_203_lock.png" else "glyphicons_043_group.png"

      val groupTags = group.getTags
      ".tags_group" #> groupTags.map((tag: String) => {
        ".tags_group *" #> tag
      }) &
        ".inf_left_groups [src]"#> ("/images/"+picName)&
        "a *" #> group.name.is &
        "a [href]" #> ("/groups/"+group.id)&
        ".description_group *"#> group.getDescription
    }
    }
  }

  def action = {
    "#add_group" #> add

  }

  def add:CssSel = {
    var newGroup = Group.create
    var public = ""

    def saveMe(){
      newGroup.validate match {
        case Nil => {
          newGroup = newGroup.owner(User.currentUserUnsafe)
          if (public != "false") newGroup.isOpen(true)
          if(newGroup.save) S.notice(S.?("approve.list.group") + newGroup.name)
          else S.warning(newGroup.name + " isn't added")
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

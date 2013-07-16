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
    val groupList =if(User.currentUserUnsafe.superUser.is) Group.findAll().toList else {User.currentUserUnsafe.groups.toList ::: Group.findAll(By(Group.isOpen,true))}

    ".container_groups" #> groupList.toSet.map{ group:Group => {

      //      def deleteGroup(): JsCmd = {
      //        if(groupService.removeGroup(group)){
      //          JsCmds.Replace(group.id.is.toString, NodeSeq.Empty)
      //        } else JsCmds.Alert("Group is not removed! Please check logs for details")
      //      }
      //      if (User.currentUser.open_!.superUser){
      //        ".container_groups [id]"#> group.id.is &
      //          ".container_groups *" #>
      //            ((n: NodeSeq) => SHtml.ajaxForm(
      //              ("a *" #> group.name.is &
      //                "a [href]" #> ("/groups/"+group.id) andThen
      ////                "* *" #> SHtml.memoize(f => f ++ SHtml.hidden(deleteGroup _))).apply(n)
      //            ))
      //      }
        val picName = if(!group.isOpen.is) "glyphicons_203_lock.png" else "glyphicons_043_group.png"

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
//    var sameCategory = "true"
    var public = ""
    var tags = ""
    var description = ""

    def saveMe(){
      newGroup.validate match {
        case Nil => {
          newGroup = newGroup.owner(User.currentUserUnsafe)
          if (public != "false") newGroup.isOpen(true)
          groupService.createGroup(newGroup, tags, description) match {
            case Full(g) => {
              S.notice("Added group: " + g.name)
//              if (sameCategory != "false"){
//                val newCategory = Category.create.name(g.name.is).linkedGroup(g)
//                newCategory.validate match {
//                  case Nil => {
//                    newCategory.save()
//                    S.notice("Added group and category: " + g.name + " is added")
//                  }
//                  case xs =>
//                    xs.foreach(f => S.error(f.msg))
//                }
//              } else{
//                S.notice("Added group: " + g.name)
//              }
            }
            case Failure(msg, _, _) => S.error(msg)
            case _ => S.error("Group is not created")
          }
        }
        case xs =>
          xs.foreach(f => S.error(f.msg))
      }
    }
    "name=name" #> SHtml.onSubmit(newGroup.name(_))&
      "name=description" #> SHtml.onSubmit(description =_)&
//      "name=sameCategory" #> SHtml.onSubmit(sameCategory = _) &
      "name=public" #> SHtml.onSubmit(public = _) &
      "name=tags_edit" #> SHtml.onSubmit(tags = _) &
      "type=submit" #> SHtml.onSubmitUnit(saveMe)
  }
}

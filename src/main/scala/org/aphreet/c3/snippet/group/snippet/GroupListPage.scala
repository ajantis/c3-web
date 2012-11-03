package org.aphreet.c3.snippet.group.snippet

import com.ifunsoftware.c3.access.C3System
import org.aphreet.c3.lib.DependencyFactory._
import org.aphreet.c3.service.GroupService
import net.liftweb.common.{Empty, Full, Logger}
import xml.{Text, NodeSeq}
import org.aphreet.c3.model.{C3Resource, UserGroup, Group, User}
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.util.BindHelpers._
import net.liftweb.mapper.By
import net.liftweb.http.js.{JsCmds, JsCmd}
import org.aphreet.c3.snippet.{FileUploadDialog, CreateDirectoryDialog}

/**
 * @author Dmitry Ivanov (mailto: Dmitry.Ivanov@reltio.com)
 *         Reltio, Inc.
 */
class GroupListPage {

  val c3 = inject[C3System].open_!

  val groupService = inject[GroupService].open_!

  val logger = Logger(classOf[GroupListPage])

  def list(html: NodeSeq) : NodeSeq = {

    val groupList = User.currentUser.open_!.groups.toList

    groupList.flatMap(group =>
      bind("group", html, "name" -> <a href={"/groups/"+group.id}>{group.name}</a>,
        "owner" -> group.owner.obj.map(usr => usr.email.is).openOr("<unknown>"),
        "delete" -> {SHtml.ajaxSubmit("Delete",
          () => {

            // TODO with Alert + Promt a-la "Are you sure you wanna delete this group??"
            if(group.delete_!) Alert("Group "+group.name.is+" deleted.")
            else Alert("Group "+group.name.is+" NOT deleted.")
            //JE.JsRaw("alert('Group "+group.name.is+" deleted.'); location.reload(true)").cmd
          }
        )})
    )

  }

  def add(form: NodeSeq) : NodeSeq = {

    val invokedAs = S.invokedAs
    var group = Group.create


    def newGroup(form: NodeSeq): NodeSeq = {
      def saveMe(): Unit = {
        group.validate match {
          case Nil => {

            group.save

            // Linking group owner with a new Group in DB
            UserGroup.join(User.find(By(User.id,group.owner)).open_!,group)

            groupService.createGroupMapping(group.id.is.toString)
            S.notice("Added group: " + group.name); S.redirectTo("/groups")
          }
          case xs => S.error(xs) ; S.mapSnippet(invokedAs, newGroup)

        }
      }

      bind("group", form,
        "name" -> group.name.toForm,
        "owner" -> group.owner.toForm,
        "submit" -> SHtml.submit("add", saveMe))
    }

    newGroup(form)
  }


  object selectedResources extends RequestVar[scala.collection.mutable.Set[String]](scala.collection.mutable.Set())
  object actionCommand extends RequestVar[Command](DeleteSelectedResources)

  def view(html: NodeSeq) : NodeSeq = {

    val groupdir = "/"+S.param("groupdirectory").openOr("")

    S.param("groupname") match {
      case Full(groupname) => {
        Group.find(By(Group.name,groupname)) match {
          case Full(group) => {

            def doCommand(cmd: Command): JsCmd = {
              cmd match {
                case DeleteSelectedResources => deleteSelected
                case _ => Alert("Error: unknown action.")
              }
            }

            def deleteSelected: JsCmd = {
              for (resName <- selectedResources){
                try {
                  c3.deleteFile(groupname+"/files/"+groupdir.tail + "/"+ resName)
                }
                catch {
                  case e: Exception => Alert(e.toString) & JsCmds.RedirectTo("")
                }
              }
              Alert("Selected resources were deleted: "+selectedResources.mkString(",")) & JsCmds.RedirectTo("")
            }

            bind("group", html,
              "selectAction" -> SHtml.ajaxSelectObj[Command]( List((DeleteSelectedResources,"Delete selected")), Full(DeleteSelectedResources), (cmd: Command) =>
              { actionCommand.set(cmd); JsCmds.Noop }
              ),
              "name" -> group.name,
              "owner" -> {group.owner.obj.map(usr => usr.email.is) openOr "unknown"},
              "create_dir" -> ( (ns: NodeSeq) => new CreateDirectoryDialog().button(ns , (if(S.uri.contains("/group/")) Full(S.uri.split("/group/").last+"/") else Empty) ) ),
              "upload_file" -> ( (ns: NodeSeq) => new FileUploadDialog().button(ns )),/*  temp fix  , (if(S.uri.contains("/group/")) Full(S.uri.split("/group/").last+"/") else Empty) ) ), */
              "linkup" -> {
                val link = {
                  if(groupdir!="/") {
                    "/" + groupdir.split('/').tail.reverse.tail.reverse.mkString("/") match {
                      case "/" => ""
                      case s => s
                    }
                  }
                  else ""
                }
                //SHtml.link("/group/"+ groupname + link,()=>{},Text("../"))
                <a href={"/group/"+ groupname + "/files" + link}>../</a>
              },
              "doAction" -> ( (ns: NodeSeq) => SHtml.ajaxButton(ns.text, () => doCommand(actionCommand.is) ) ),
              "childs" -> {

                (ns: NodeSeq) => group.getChildren(groupdir).sortWith(_.resourceType <= _.resourceType).flatMap(child => {

                  val path = "/" + groupname+"/files/"+ {groupdir.tail match {
                    case "" => ""
                    case str => str + "/"
                  } } +  child.name

                  val file = c3.getFile(path)
                  val created = file.date

                  val url = "/group" + path

                  bind("child",ns,
                    "name" -> <a href={url}>{child.name}</a>,
                    "type" -> child.resourceType,
                    "icon" -> { child.resourceType match {
                      case C3Resource.C3_FILE => <img src="/images/icons/document.gif"/>
                      case _ => <img src="/images/icons/folder.gif"/>
                    }},
                    "created" -> created.toString,
                    "delete" -> SHtml.ajaxButton("Delete",() => {

                      try {
                        c3.deleteFile("/" + groupname+"/files/"+groupdir.tail + "/"+ child.name)
                        Alert("Resource "+ child.name +" deleted.")
                      }
                      catch {
                        case e: Exception => Alert(e.toString)
                      }

                    } ),
                    "select" -> SHtml.ajaxCheckbox(false, { checked =>
                      if(checked)
                        selectedResources += child.name
                      else
                        selectedResources -= child.name
                      JsCmds.Noop
                    })
                  ) }
                ):NodeSeq

              }
            )
          }
          case Empty => Text("Group "+groupname+" wasn't found in C3")
          case _ => Text("WTF with groupname?")
        }
      }
      case Empty => {
        Text("No group was choosed")
      }
      case _ => Text("Something bad has happend")
    }

  }

  def groupMenu(html: NodeSeq) : NodeSeq = {

    S.param("groupname") match {
      case Full(name) => {

        bind("group",html,
          "name" -> name,
          "overview_link" -> <a href={"/group/"+name}>Overview</a>,
          "files_link" -> <a href={"/group/"+name+"/files"}>Files</a>,
          "wiki_link" -> <a href={"/group/"+name+"/wiki"}>Wiki</a>,
          "admin_link" -> {Group.find(By(Group.name,name)) match {
            case Full(group) => {
              User.currentUser match {
                case Full(user) if(user.id.is == group.owner.is) => <a href={"/group/"+name+"/admin"}>Admin</a>
                case _ => NodeSeq.Empty
              }
            }
            case _ => NodeSeq.Empty
          } }
        )
      }
      case _ => NodeSeq.Empty
    }
  }

  def groupOverview(html: NodeSeq) : NodeSeq = {
    S.param("groupname") match {
      case Full(name) => {
        Group.find(By(Group.name, name)) match {
          case Full(group) => {
            bind("group", html,
              "name" -> group.name,
              "description" -> group.description,
              "owner" -> {group.owner.obj.map(_.email.is).getOrElse("UNKNOWN")},
              "users" -> { (ns: NodeSeq) => group.users.flatMap(
                (user: User) =>
                  bind("user",ns,
                    "email"-> user.email
                  )
              ):NodeSeq }

            )
          }
          case _ => NodeSeq.Empty
        }
      }
      case _ => NodeSeq.Empty
    }
  }


  def resourceProperties(html: NodeSeq) : NodeSeq = {

    S.param("groupdirectory") match {
      case Full(groupDirName) if(!S.param("groupname").isEmpty) =>

        val groupName = S.param("groupname").open_!
        bindNodeParameters(html, groupName, groupDirName)

      case _ =>  // Directory name is not defined as parameter at the request
        S.param("groupfile") match {

          case Full(groupFileName) if(!S.param("groupname").isEmpty) => {
            val groupName = S.param("groupname").open_!
            bindNodeParameters(html, groupName, groupFileName)

          }
          case _ => NodeSeq.Empty   // File name is not defined as parameter at the request
        }
    }
  }

  private def bindNodeParameters(html:NodeSeq, groupName:String, groupPath:String):NodeSeq = {
    try{
      val file = c3.getFile(pathInGroup(groupName, groupPath))

      val md = file.metadata
      val createDate = file.date
      val tags = md.getOrElse("tags", "").split(",").toList

      bind("currentResource", html,
        "name" -> groupPath,
        "owner" -> "owner2",
        "created" -> createDate.toString,             // TODO List(tags) is a STUB for tag display test
        "tags_list" -> {(ns: NodeSeq) => tags.flatMap( tag =>
          bind("tag", ns, "name" -> tag)) : NodeSeq
        }
      )
    }catch{
      case e => {
        S.error(e.toString)
        NodeSeq.Empty
      }
    }
  }

  def pathInGroup(groupName:String, path:String):String = {
    "/" + groupName + "/files/" + path
  }

}

abstract class Command
case object DeleteSelectedResources extends Command
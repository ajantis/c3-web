/**
 * Copyright (c) 2011, Dmitry Ivanov
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *

 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following disclaimer
 * in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the IFMO nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.aphreet.c3.snippet

import net.liftweb.util.BindHelpers._
import org.aphreet.c3.model._
import net.liftweb.common.{Box, Logger, Full, Empty}
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.mapper.By
import java.net.URLEncoder
import net.liftweb.util.TimeHelpers
import xml.{Node, Text, NodeSeq}
import org.apache.commons.lang.time.DateUtils
import java.util.Date
import java.text.SimpleDateFormat

import javax.activation.MimetypesFileTypeMap
import org.aphreet.c3.apiaccess.{C3ClientException, C3Client}
import org.apache.commons.httpclient.util.URIUtil
import net.liftweb.widgets.uploadprogress.UploadProgress
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http._

class GroupForm {


  val logger = Logger(classOf[GroupForm])

  def list(html: NodeSeq) : NodeSeq = {

    val groupList = User.currentUser.open_!.groups.toList

    groupList.flatMap(group =>
      bind("group", html, "name" -> <a href={"/group/"+group.name}>{group.name}</a>,
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

            C3Client().createGroupMapping(group)
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
                  C3Client().delete(groupname+"/files/"+groupdir.tail + "/"+ resName)
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

                   val childMetadata = C3Client().getNodeMetadata(groupname+"/files/"+ {groupdir.tail match {
                     case "" => ""
                     case str => str + "/"
                   } } +  child.name)

                   val DATE_FORMAT_8601 = "yyyy-MM-dd'T'HH:mm:ss"

                   val format: SimpleDateFormat = new SimpleDateFormat("dd/MM/yyyy")

                   val created: Date = ((childMetadata \\ "resource")(0) \\ "@createDate").text match {
                     case "" => TimeHelpers.now
                     case str => DateUtils.parseDate(str.split('.').head, Array(DATE_FORMAT_8601))
                   }

                   bind("child",ns,
                      "name" -> {

                        val url =
                          if(child.resourceType=="directory") {
                            if(groupdir.tail.isEmpty){
                              "/group/"+groupname + "/files/"+ child.name
                            }else{
                              "/group/"+groupname + "/files/" + groupdir.tail + "/"+ child.name
                            }
                          }
                          else {
                            if(groupdir.tail.isEmpty){
                              "/download/"+groupname + "/files/"+ child.name
                            }else{
                              "/download/"+groupname + "/files/" + groupdir.tail + "/"+ child.name
                            }
                          }
                         <a href={url}>{child.name}</a>

                      },
                      "type" -> child.resourceType,
                      "icon" -> { child.resourceType match {
                        case C3Resource.C3_FILE => <img src="/images/icons/document.gif"/>
                        case _ => <img src="/images/icons/folder.gif"/>
                      }},
                      "created" -> format.format(created),
                      "delete" -> SHtml.ajaxButton("Delete",() => {

                        try {
                          C3Client().delete(groupname+"/files/"+groupdir.tail + "/"+ child.name)
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

  // the request-local variable that hold the file parameter
  private object theUpload extends RequestVar[Box[FileParamHolder]](Empty)
  private object theUploadPath extends RequestVar[Box[String]](Empty)
  private object theCreateDirectoryPath extends RequestVar[Box[String]](Empty)

  def uploadHere(xhtml: NodeSeq): NodeSeq = {
      if (S.get_? || theUploadPath.isEmpty || theUpload.isEmpty) {

        bind("ul", chooseTemplate("choose", "get", xhtml),
          "file_upload" -> SHtml.fileUpload(ul => theUpload(Full(ul))),
          "filename" -> SHtml.text("",(filename: String) => theUploadPath(if(S.uri.contains("/group/")) Full(S.uri.split("/group/").last+"/" + filename) else Empty)),
          //"uploadBar" -> ( (ns: NodeSeq) => UploadProgress.head(ns) ),
          "submitfile" -> SHtml.submit("Upload",() => { S.redirectTo(S.uri) }),
          AttrBindParam("uploadFileStyle", Text("display: none;"), "style"))
      }
      else {

        // this simple technique helps to predict uploaded file's type by it's name
        val mimeType: String = new MimetypesFileTypeMap().getContentType(theUpload.is.open_!.fileName)

        try {

          theUpload.is.open_! match {
            case file: OnDiskFileParamHolder => logger error("File: " + file.localFile.getAbsolutePath() )
            case _ => {}
          }

          C3Client().uploadFile( URIUtil.decode(theUploadPath.is.open_!),theUpload.is.map(v => v.file).open_!, Map("content.type" -> mimeType))

          S.notice(
            <p>File {theUpload.is.map(v => v.fileName).open_!} successfully uploaded</p>
             ++ SHtml.ajaxButton("Refresh", () => JsCmds.RedirectTo("")) //++ bind("ul",chooseTemplate("choose", "post", xhtml), "uploadBar" -> ( (ns: NodeSeq) => UploadProgress.head(ns) ) )
          )
        }
        catch {
          case e: C3ClientException => S.error(e.toString)
        }

        NodeSeq.Empty
        /*
        bind("ul", chooseTemplate("choose", "post", xhtml),
          "filename" -> theUpload.is.map(v => Text(v.fileName)),
          "refresh" -> ((ns: NodeSeq) => SHtml.ajaxButton(ns.text, () => JsCmds.RedirectTo(""))),
          AttrBindParam("uploadFileStyle", Text(""), "style")
        ) */
      }
  }




  def groupMenu(html: NodeSeq) : NodeSeq = {

     S.param("groupname") match {
       case Full(name) => {

         bind("group",html,
         "name" -> name,
         "overviewLink" -> <a href={"/group/"+name}>Overview</a>,
         "filesLink" -> <a href={"/group/"+name+"/files"}>Files</a>,
         "wikiLink" -> <a href={"/group/"+name+"/wiki"}>Wiki</a>,
         "adminLink" -> {Group.find(By(Group.name,name)) match {
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
          try {
            val md = C3Client().getNodeMetadata(S.param("groupname").open_! + "/files/" + groupDirName)
            val createDate = ((md \\ "resource")(0) \ "@createDate").text
            val tags =
              ((md \\ "metadata")(0) \\ "element").toList.filter( (node: Node) => {(node \ "@key").text == "tags" }) match {
                case List() => ""
                case tagsElems => (tagsElems(0) \ "value").text
              }


            bind("currentResource", html,
              "name" -> groupDirName,
              "owner" -> "owner1",
              "created" -> createDate,                                               // TODO List(tags) is a STUB for tag display test
              "tags_list" -> {(ns: NodeSeq) => List("tag1","tag2","tag3").flatMap( tag =>
                bind("tag", ns, "name" -> tag)) : NodeSeq
              }

            )
          }
          catch {
            case e: Exception => {
              S.error(e.toString)
              NodeSeq.Empty
            }
          }

        case _ =>  // Directory name is not defined as parameter at the request
            S.param("groupfile") match {

                    case Full(groupFileName) if(!S.param("groupname").isEmpty) => {
                      try {
                        val md = C3Client().getNodeMetadata(S.param("groupname").open_! + "/files/" + groupFileName)
                        val createDate = ((md \\ "resource")(0) \ "@createDate").text

                        val tags =
                        ((md \\ "metadata")(0) \\ "element").toList.filter( (node: Node) => {(node \ "@key").text == "tags" }) match {
                          case List() => ""
                          case tagsElems => (tagsElems(0) \ "value").text
                        }

                        bind("currentResource", html,
                          "name" -> groupFileName,
                          "owner" -> "owner2",
                          "created" -> TimeHelpers.now.toString,             // TODO List(tags) is a STUB for tag display test
                          "tags_list" -> {(ns: NodeSeq) => List("tag11","tag223","tag323").flatMap( tag =>
                            bind("tag", ns, "name" -> tag)) : NodeSeq
                          }

                        )
                      }
                      catch {
                        case e: Exception => {
                          S.error(e.toString)
                          NodeSeq.Empty
                        }
                      }
                    }
                    case _ => NodeSeq.Empty   // File name is not defined as parameter at the request
            }
      }
  }

}

abstract class Command
case object DeleteSelectedResources extends Command
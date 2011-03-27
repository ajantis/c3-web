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

import org.aphreet.c3.apiaccess.C3Client
import net.liftweb.util.BindHelpers._
import xml.{Text, NodeSeq}
import net.liftweb.mapper.By
import org.aphreet.c3.model._
import net.liftweb.common.{Box, Logger, Full, Empty}
import net.liftweb.http.{FileParamHolder, RequestVar, SHtml, S}
import net.liftweb.util.SecurityHelpers
import net.liftweb.http.js.JsCmds.Alert

class GroupForm {


  val logger = Logger(classOf[GroupForm])

  def list(html: NodeSeq) : NodeSeq = {

    val groupList = User.currentUser.open_!.groups.toList

    groupList.flatMap(group =>
      bind("group", html, "name" -> <a href={"/group/"+group.name}>{group.name}</a>,
        "owner" -> group.owner.obj.map(usr => usr.email.is).openOr("<unknown>"))
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
            UserGroup.join(User.find(By(User.id,group.owner.is)).open_!,group)

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


  def view(html: NodeSeq) : NodeSeq = {

    val groupdir = "/"+S.param("groupdirectory").openOr("")


    S.param("groupname") match {
      case Full(groupname) => {
        Group.find(By(Group.name,groupname)) match {
          case Full(group) => {
            bind("group", html,
              "name" -> group.name,
              "owner" -> {group.owner.obj.map(usr => usr.email.is) openOr "unknown"},
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
              "childs" -> {

                 (ns: NodeSeq) => group.getChildren(groupdir).flatMap(child =>
                   bind("child",ns,
                      "name" -> {

                        val url =

                          if(groupdir.tail.isEmpty){
                            "/group/"+groupname + "/files/"+ child.name
                          }else{
                            "/group/"+groupname + "/files/" + groupdir.tail + "/"+ child.name
                          }

                         <a href={url}>{child.name}</a>

                      },
                      "type" -> child.resourceType,
                      "delete" -> SHtml.ajaxButton("Delete",() => {

                        try {
                          C3Client().delete(groupname+"/"+groupdir.tail + "/"+ child.name)
                          Alert("Resource "+ child.name +" deleted.")
                        }
                        catch {
                          case e: Exception => Alert(e.toString)
                        }

                      } )
                  )):NodeSeq
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
      if (S.get_? || theUploadPath.isEmpty) {

        bind("ul", chooseTemplate("choose", "get", xhtml),
          "file_upload" -> SHtml.fileUpload(ul => theUpload(Full(ul))),
          "filename" -> SHtml.text("",(filename: String) => theUploadPath(if(S.uri.contains("/group/")) Full(S.uri.split("/group/").last.split("/files").mkString +"/"+filename) else Empty)),
          "submitfile" -> SHtml.submit("Upload",() => { S.redirectTo(S.uri) }),
          AttrBindParam("uploadFileStyle", Text("display: none;"), "style"))
      }
      else {

        C3Client().uploadFile( theUploadPath.is.open_!,theUpload.is.map(v => v.file).open_!)

        bind("ul", chooseTemplate("choose", "post", xhtml),
          "filename" -> theUpload.is.map(v => Text(v.fileName)),
          AttrBindParam("uploadFileStyle", Text(""), "style")
        )
      }
  }

  def createDirectoryHere(xhtml: NodeSeq): NodeSeq = {
     if (S.get_? || theCreateDirectoryPath.isEmpty) {

        bind("ul", chooseTemplate("choose", "get", xhtml),
          "dirname" -> SHtml.text("",(dirname: String) => theCreateDirectoryPath(if(S.uri.contains("/group/")) Full(S.uri.split("/group/").last.split("/files").mkString +"/"+dirname) else Empty)),
          "submitdirectory" -> SHtml.submit("Create",() => { S.redirectTo(S.uri) }),
          AttrBindParam("uploadDirStyle", Text("display: none;"), "style"))
      }
      else {

        C3Client().createDir(theCreateDirectoryPath.is.open_!)

        bind("ul", chooseTemplate("choose", "post", xhtml),
          "dirname" -> theCreateDirectoryPath.is.map(v => Text(v)),
          AttrBindParam("uploadDirStyle", Text(""), "style")
        )
      }
  }

}
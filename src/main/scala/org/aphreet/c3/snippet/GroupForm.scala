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
                      "type" -> child.resourceType
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

  /**
   * Bind the appropriate XHTML to the form
   */
  def upload(xhtml: NodeSeq): NodeSeq = {


      if (S.get_?) bind("ul", chooseTemplate("choose", "get", xhtml),
                        "file_upload" -> SHtml.fileUpload(ul => theUpload(Full(ul))),
                        "file_upload_path" -> SHtml.text("",(path: String) => theUploadPath(Full(path))))
      else {

        C3Client().uploadFile( theUploadPath.is.open_!,theUpload.is.map(v => v.file).open_!)

        bind("ul", chooseTemplate("choose", "post", xhtml),
          "file_name" -> theUpload.is.map(v => Text(v.fileName)),
          "mime_type" -> theUpload.is.map(v => Box.legacyNullTest(v.mimeType).map(Text).openOr(Text("No mime type supplied"))), // Text(v.mimeType)),
          "length" -> theUpload.is.map(v => Text(v.file.length.toString)),
          "md5" -> theUpload.is.map(v => Text(SecurityHelpers.hexEncode(SecurityHelpers.md5(v.file))))
        )
      }
  }


  import net.liftweb.http.js.JsCmds._
  def uploadHere(xhtml: NodeSeq): NodeSeq = {
      if (S.get_?) {

        bind("ul", chooseTemplate("choose", "get", xhtml),
          "file_upload" -> SHtml.fileUpload(ul => theUpload(Full(ul))),
          "filename" -> SHtml.text("",(filename: String) => theUploadPath(if(S.uri.contains("/group/")) Full(S.uri.split("/group/").last +"/"+filename) else Empty)),
          "submitfile" -> SHtml.submit("upload",() => { S.redirectTo(S.uri +"/files") }))
      }
      else {

        C3Client().uploadFile( theUploadPath.is.open_!,theUpload.is.map(v => v.file).open_!)

        bind("ul", chooseTemplate("choose", "post", xhtml),
          "filename" -> theUpload.is.map(v => Text(v.fileName))
        )
      }
  }

}
package org.aphreet.c3.snippet

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
 

import _root_.net.liftweb._
import http._
import SHtml._
import js._
import JsCmds._
import js.jquery.JqJsCmds
import JqJsCmds._
import common._
import util._
import Helpers._
import javax.activation.MimetypesFileTypeMap
import xml.{Text, NodeSeq}
import java.net.URLEncoder
import org.aphreet.c3.apiaccess.{C3ClientException, C3Client}

trait AbstractFormDialog {

  val templateName: String
  protected  object theCurrentPath extends SessionVar[Box[String]](Empty)

  def unblockForm(xhtml: NodeSeq) =
    bind("form",xhtml,
      "close" -> ((b: NodeSeq) => <button onclick={Unblock.toJsCmd}>{b}</button>))

  def button(in: NodeSeq, path: Box[String]) =
      SHtml.ajaxButton(in,
               () => {
                 theCurrentPath.set(path)
                 S.runTemplate(List(templateName)).
                   map(ns => ModalDialog(ns)) openOr
                   Alert("Couldn't find "+templateName+" template")
               })

  def form(xhtml: NodeSeq): NodeSeq

}

class FileUploadDialog extends AbstractFormDialog {

  val templateName = "_jsdialog_fileupload"


  private object theFileName extends RequestVar[Box[String]](Empty)
  private object theFileUpload extends RequestVar[Box[FileParamHolder]](Empty)

  private def uploadFile(): JsCmd = {

    theFileName.is match {
      case Full(some) => Alert("Stub for file upload!")
      case _ => Alert("File is empty :(") & Unblock
    }

  }

  override def form(xhtml: NodeSeq) = {

    SHtml.ajaxForm(
      bind("file", xhtml,
        "name" -> SHtml. text("", (name: String) => if(name != "") theFileName.set(Full(name)) ),
        "file_upload" -> SHtml.fileUpload(ul => theFileUpload.set(Full(ul))),
        "submit" ->((ns: NodeSeq) => SHtml.hidden(uploadFile _) ++ SHtml.submit(ns.text, () => {} ))
      )
    )
  }


}

class CreateDirectoryDialog extends AbstractFormDialog {

  val templateName = "_jsdialog_createdirectory"


  private object theDirectoryName extends RequestVar[Box[String]](Empty)


  private def createDirectory(): JsCmd = {
      if(!theCurrentPath.isEmpty)
        theDirectoryName.is match {
          case Full(name) => {

            try {
              C3Client().createDir(
                theCurrentPath.get.open_! + "/" + theDirectoryName.get.open_!
              ) match {
                  case true => Unblock & Alert("Directory created.") & RedirectTo("")
                  case _ => Unblock & Alert("Ooops. Directory wasn't created!")
              }
            }
            catch {
              case e: C3ClientException => Alert(e.toString)
            }

          }
          case _ => Alert("Please, enter directory name.")
        }
      else Unblock & Alert("Internal error: unknown current path.")
  }


  override def form(xhtml: NodeSeq) = {

    SHtml.ajaxForm(
      bind("dir", xhtml,
        "name" -> SHtml.text("", (name: String) => if(name != "") theDirectoryName.set(Full(name)) ),
        "submit" ->((ns: NodeSeq) => SHtml.hidden(createDirectory _) ++ SHtml.submit(ns.text, () => {} ))
      )
    )
  }

}

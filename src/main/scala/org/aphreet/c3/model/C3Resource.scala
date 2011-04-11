package org.aphreet.c3.model

import org.aphreet.c3.apiaccess.C3Client
import net.liftweb.util.TimeHelpers
import java.util.Date
import java.text.SimpleDateFormat
import net.liftweb.common.Logger

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
 
 
 
abstract class C3Resource {
  val resourceType : String
  val name : String
}

object C3Resource {
  val C3_DIRECTORY = "directory"
  val C3_FILE = "file"
  val logger = Logger(classOf[C3Resource])

  def get(group : Group, relativePath: String ) : Option[C3Resource] = {

      try {
        val fileMD = C3Client().getNodeMetadata(group.name.is+"/files/"+relativePath)
        if(! (fileMD \\ "metadata").isEmpty ) {

          val createDate : Date = (fileMD \\ "resource")(0).attribute("created") match {
            case Some(dateStr) => {
              val fmt = new SimpleDateFormat("YYYY-MM-DDThh:mm:ss.SSSz")
              fmt.parse(dateStr.toString)
            }
            case None => TimeHelpers.now
          }

          ((fileMD \\ "metadata")(0) \\ "element").find( _.attribute("key").getOrElse("").toString == "content.type" ) match {
              case Some(element) => (element \\ "value").text  match {

                  case "application/x-c3-directory" => Some(Catalog(group = group, name = relativePath.split("/").last, createDate =  createDate))

                  case fileType => Some(File(group = group, fullpath = relativePath, fileType = fileType,create = createDate ))

              }
              case None => {
                  throw new Exception("Content.TYPE of "+relativePath+" is empty!"+ ((fileMD \\ "metadata")(0) \\ "element").toString)
                  //None
              }
          }
        }
        else {
          throw new Exception("Metadata of "+relativePath+" is empty!")
          //None
        }
      }
      catch {
        case e: Exception => {
          logger error e.getMessage
          None
        }
      }



  }

}

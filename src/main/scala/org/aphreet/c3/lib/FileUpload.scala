package org.aphreet.c3.lib

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
 
 

import net.liftweb.http.rest._
import net.liftweb.json._
import JsonDSL._
import net.liftweb.http.{S, InMemoryResponse, JsonResponse}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.common.Box

// for ajax file upload
object FileUpload extends RestHelper {
  serve {
    case "upload" :: "file" :: Nil Post req => {
      println("uploaded "+req.uploadedFiles)
      val ojv: Box[JValue] =
        req.uploadedFiles.map(fph => ("name" -> fph.fileName) ~
                              ("type" -> fph.mimeType) ~
                              ("size" -> fph.file.length)).headOption

      val ajv = ("name" -> "n/a") ~ ("type" -> "n/a") ~ ("size" -> 0L) ~ ("yak" -> "brrrr")

      val ret = ojv openOr ajv

      // This is a tad bit of a hack, but we need to return text/plain, not JSON
      val jr = JsonResponse(ret).toResponse.asInstanceOf[InMemoryResponse]
      InMemoryResponse(jr.data, ("Content-Length", jr.data.length.toString) ::
                       ("Content-Type", "text/plain") :: S.getHeaders(Nil),
                       S.responseCookies, 200)
    }
  }
}
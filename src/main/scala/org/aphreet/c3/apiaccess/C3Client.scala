package org.aphreet.c3.apiaccess

import net.liftweb.util.Props
import org.apache.commons.httpclient.methods._
import multipart._
import org.aphreet.c3.model.Group
import net.liftweb.common.Logger
import java.text.SimpleDateFormat
import java.util.Date
import org.apache.commons.httpclient._
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import xml.{XML, NodeSeq}
import java.io.{ByteArrayInputStream, InputStream}

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

object C3Client {
  def apply() = new C3Client()
}

class C3Client  {

  val requestUri = "/rest/fs"

  //val url = host + requestUri

  val url = "http://c3.aphreet.org:7373" + requestUri

  val logger = Logger(classOf[C3Client])

  val httpClient = new HttpClient()

  val C3_FS_API_URL = Props.get("c3fsapi_url") openOr("http://localhost:7373/rest/fs/")

  // Domain
 // c3web   95b39c16b3b6316a938ea5acbdba24a3
  def createGroupMapping(group: Group): Boolean = {

    val createRequest = new PostMethod(C3_FS_API_URL + group.name.is)

    addAuthHeader(createRequest, "/rest/fs/" + group.name.is)

    createRequest.addRequestHeader("x-c3-nodetype", "directory")

    try{
      val status = httpClient.executeMethod(createRequest)
      status match {
        case HttpStatus.SC_CREATED => {
           true
        }
        case _ => {
          throw new Exception(("Failed to make group-directory mapping, message: " + createRequest.getResponseBodyAsString))
          false
        }
      }
    }
  }
  /*
  def listGroupFiles(group: Group): List[(String, FileType)] = {
    val getRequest = new GetMethod(C3_FS_API_URL + group.name.is)

    val groupCatalog = {
      httpClient.executeMethod(getRequest)
      getRequest.getResponseBodyAsString.asInstanceOf[NodeSeq]
    }

    val nodes = ((groupCatalog \\ "directory")(0) \\ "nodes")(0) \\ "node"

    {for(node <- nodes) yield (
      (node \ "@name") text ,
        if((((node \ "@leaf") text) toBoolean)){
         File()
        }else{
         Directory()
        }
      )}.toList

  } */

  def createDir(path: String): Boolean = {

    val createRequest = new PostMethod(C3_FS_API_URL + path)

    addAuthHeader(createRequest, "/rest/fs/" + path)

    createRequest.addRequestHeader("x-c3-nodetype", "directory")

    try{
      val status = httpClient.executeMethod(createRequest)
      status match {
        case HttpStatus.SC_CREATED => {
           true
        }
        case _ => {
          throw new Exception(("Failed to create directory: "+path+", message: " + createRequest.getResponseBodyAsString))
          false
        }
      }
    }
  }

  def listResources(pathToDirectory: String) = {

    val getRequest = new GetMethod(C3_FS_API_URL + pathToDirectory)

    addAuthHeader(getRequest, "/rest/fs/" + pathToDirectory)

    val catalog = {
      httpClient.executeMethod(getRequest)
      XML.load(getRequest.getResponseBodyAsStream)
    }

    val nodes = ((catalog \\ "directory")(0) \\ "nodes")(0) \\ "node"

    nodes

  }


  def uploadFile( path:String, fileByteArray:Array[Byte] ) = {
    val fileBytePartSource = new ByteArrayPartSource(fileByteArray)
    writeData(path, new FilePart("data", fileBytePartSource), Map[String, String]())
  }

  private def writeData(path:String, filePart:FilePart, metadata:Map[String, String]) = {
    val postMethod = new PostMethod(C3_FS_API_URL + path)

    addAuthHeader(postMethod, "/rest/fs/" + path)

    val parts:Array[Part] = (filePart ::
            metadata.map(e => new StringPart(e._1, e._2, "UTF-8")).toList).toArray

    postMethod.setRequestEntity(new MultipartRequestEntity(parts, postMethod.getParams))

    try{
      val status = httpClient.executeMethod(postMethod)
      status match {
        case HttpStatus.SC_CREATED => {
        }
        case _ =>
          println(postMethod.getResponseBodyAsString)
          throw new Exception(("Filed to post resource "+ path +" , code " + status).asInstanceOf[String])
      }
    }finally {
      postMethod.releaseConnection
    }
  }

  def createGroup (groupName : String) = createDir(groupName)


  private val domain = Props.get("c3_domain_name") openOr "anonymous"
  private val secret = Props.get("c3_domain_secret") openOr ""

  private def addAuthHeader(method:HttpMethodBase, resource:String) = {
    if(domain != "anonymous"){

      val dateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss z")

      val dateString = dateFormat.format(new Date())

      val hashBase = resource + dateString + domain

      val hash = hmac(secret, hashBase)

      val header = new Header("x-c3-sign", hash)
      method.addRequestHeader(header)

      val domainHeader = new Header("x-c3-domain", domain)
      method.addRequestHeader(domainHeader)

      val dateHeader = new Header("x-c3-date", dateString)
      method.addRequestHeader(dateHeader)
    }
  }

  private def hmac(key:String, input:String):String = {

    val mac = Mac.getInstance("HmacSHA256")

    val secret = new SecretKeySpec(key.getBytes, "HmacSHA256")
    mac.init(secret)

    val digest = mac.doFinal(input.getBytes("UTF-8"));

    val hexString = new StringBuilder

    for (b <- digest) {
      if ((0xFF & b) < 0x10) {
        hexString.append("0").append(Integer.toHexString((0xFF & b)))
      } else {
        hexString.append(Integer.toHexString((0xFF & b)))
      }
    }

    hexString.toString
  }

}


class ByteArrayPartSource(val data:Array[Byte]) extends PartSource {

  override def createInputStream:InputStream = {
    new ByteArrayInputStream(data)
  }

  override def getFileName:String = {
    return "array"
  }

  override def getLength:Long = data.length
}

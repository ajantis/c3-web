/**
 * Copyright (c) 2011, Dmitry Ivanov, Mikhail Malygin
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
import xml.XML
import java.io.{ByteArrayInputStream, InputStream}

class C3Client(val host:String, val contextPath:String,  val domain:String, val secret:String)  {

  val logger = Logger(classOf[C3Client])

  val httpClient = new HttpClient()


  def createGroupMapping(group: Group): Boolean = createDir(group.name.is)

  def delete(path:String) = {
    val deleteMethod = createDeleteMethod(path)

    try{
      val status = httpClient.executeMethod(deleteMethod)
      status match {
        case HttpStatus.SC_OK => {

        }
        case _ =>
          println(deleteMethod.getResponseBodyAsString)
          throw new Exception(("Failed to delete file, code " + status).asInstanceOf[String])
      }
    }
  }

  def getNodeData(path:String):Array[Byte] = {

    val getMethod = createGetMethod(path)

    try{
      val status = httpClient.executeMethod(getMethod)
      status match {
        case HttpStatus.SC_OK => {
         getMethod.getResponseBody
        }
        case _ =>
          println(getMethod.getResponseBodyAsString)
          throw new Exception(("Failed to get resource, code " + status).asInstanceOf[String])
      }
    }finally{
      getMethod.releaseConnection();
    }
  }


  def createDir(path: String): Boolean = {

    val createRequest = createPostMethod(path)

    createRequest.addRequestHeader("x-c3-nodetype", "directory")

    try{

      val status = httpClient.executeMethod(createRequest)

      status match {
        case HttpStatus.SC_CREATED => true

        case _ => {
          logger.debug("Failed to create directory. Message: " + createRequest.getResponseBodyAsString)
          throw new C3ClientException("Failed to create directory: " + path + ". Status code is " + status)
        }
      }
    }finally {
      createRequest.releaseConnection;
    }
  }

  def listResources(pathToDirectory: String) = {

    val getRequest = createGetMethod(pathToDirectory)

    val catalog = {
      try{
        val status = httpClient.executeMethod(getRequest)
        status match{
          case HttpStatus.SC_OK =>
            if(isXmlResponse(getRequest))
              XML.load(getRequest.getResponseBodyAsStream)
            else{
              throw new C3ClientException("Failed to get resource list. Unexpected content type")
            }

          case _ => {
            logger.warn("Failed to get resource, code: " + status + " response: " + getRequest.getResponseBodyAsString)
            throw new C3ClientException("Failed to get resource")
          }
        }
      }finally {
        getRequest.releaseConnection
      }
    }

    ((catalog \\ "directory")(0) \\ "nodes")(0) \\ "node"
  }


  def uploadFile( path:String, fileByteArray:Array[Byte] ) = {
    val fileBytePartSource = new ByteArrayPartSource(fileByteArray)
    writeData(path, new FilePart("data", fileBytePartSource), Map[String, String]())
  }

  private def writeData(path:String, filePart:FilePart, metadata:Map[String, String]) = {
    val postMethod = createPostMethod(path)

    val parts:Array[Part] = (filePart ::
      metadata.map(e => new StringPart(e._1, e._2, "UTF-8")).toList).toArray

    postMethod.setRequestEntity(new MultipartRequestEntity(parts, postMethod.getParams))

    try{
      val status = httpClient.executeMethod(postMethod)
      status match {
        case HttpStatus.SC_CREATED =>
        case _ =>
          logger.debug("Failed to post resource. Response: " + postMethod.getResponseBodyAsString)
          throw new C3ClientException(("Filed to post resource "+ path +" , code " + status).asInstanceOf[String])
      }
    }finally {
      postMethod.releaseConnection
    }
  }

  def getResourceAsString(path:String):String = {
    val getMethod = createGetMethod(path)

    val status = httpClient.executeMethod(getMethod)

    status match {
      case HttpStatus.SC_OK => {
        getMethod.getResponseBodyAsString
      }
      case _ => {
        logger.debug("Failed to get resource. Response: " + getMethod.getResponseBodyAsString)
        throw new C3ClientException(("Filed to get resource "+ path +" , code " + status).asInstanceOf[String])
      }
    }
  }

  def createGroup (groupName : String) = createDir(groupName)

  private def isXmlResponse(method:HttpMethod):Boolean = {
    val contentTypeHeader = method.getResponseHeader("Content-Type")

    if(contentTypeHeader != null){
      contentTypeHeader.getValue.startsWith("application/xml")
    }else{
      false
    }
  }

  private def createPostMethod(relativePath:String):PostMethod = {
    logger.info(host + contextPath + relativePath)
    val method = new PostMethod(host + contextPath + relativePath)
    addAuthHeader(method, contextPath + relativePath)
    method
  }

  private def createGetMethod(relativePath:String):GetMethod = {
    logger.info(host + contextPath + relativePath)
        
    val method = new GetMethod(host + contextPath + relativePath)
    addAuthHeader(method, contextPath + relativePath)
    method
  }

  private def createDeleteMethod(relativePath:String):DeleteMethod = {
    logger.info(host + contextPath + relativePath)

    val method = new DeleteMethod(host + contextPath + relativePath)
    addAuthHeader(method, contextPath + relativePath)
    method
  }

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

object C3Client {
  def apply() = {

    val host = Props.get("c3_host") openOr("http://localhost:7373")

    val contextPath = Props.get("c3_context_path") openOr("/rest/fs/")

    val domain = Props.get("c3_domain_name") openOr "anonymous"

    val secret = Props.get("c3_domain_secret") openOr ""

    new C3Client(host, contextPath, domain, secret)
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

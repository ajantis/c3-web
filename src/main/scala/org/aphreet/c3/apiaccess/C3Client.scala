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
import java.io.{ByteArrayInputStream, InputStream}
import xml.{NodeSeq, XML}

import org.apache.commons.httpclient.util.URIUtil
import java.net.URLEncoder

class C3Client(val host:String, val contextPath:String, val contextRestPath:String,  val domain:String, val secret:String)  {

  val logger = Logger(classOf[C3Client])

  val httpClient = new HttpClient()

  val contextRestSearchPath = "/rest/search/" //contextRestPath + "search/"

  def createGroupMapping(group: Group): Boolean = {
    if(createDir(group.name.is)){
      createDir(group.name.is+"/files")
      createDir(group.name.is+"/wiki")
    }
    else false
  }

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

  def getResourceMetadataWithFSPath(resourceId : String): NodeSeq = {
    val getMethod = createGetRestMethod(resourceId + "?metadata")
    getMethod.addRequestHeader(new Header("x-c3-extmeta", "c3.ext.fs.path"))

    try {
      val status = httpClient.executeMethod(getMethod)

      status match {
        case HttpStatus.SC_OK => {
          XML.load(getMethod.getResponseBodyAsStream)
        }
        case _ => {
          throw new Exception(("Failed to get resource metadata, code "+ status).asInstanceOf[String])
        }
      }

    }
    finally  {
      getMethod.releaseConnection()
    }
  }


  def getResourceMetadata(resourceId : String): NodeSeq = {
    val getMethod = createGetRestMethod(resourceId + "?metadata")

    try {
      val status = httpClient.executeMethod(getMethod)

      status match {
        case HttpStatus.SC_OK => {
          XML.load(getMethod.getResponseBodyAsStream)
        }
        case _ => {
          logger info getMethod.getResponseBodyAsString
          //XML.load(getMethod.getResponseBodyAsStream)
          throw new Exception(("Failed to get resource metadata, code "+ status).asInstanceOf[String])
        }
      }

    }
    finally  {
      getMethod.releaseConnection()
    }
  }


  def getNodeMetadata(path:String): NodeSeq = {
    val getMethod = createGetMethod(URIUtil.encodeQuery(path,"UTF-8") + "?metadata")

    try{
      val status = httpClient.executeMethod(getMethod)
      status match {
        case HttpStatus.SC_OK => {
         XML.load(getMethod.getResponseBodyAsStream)
        }
        case _ => {
          XML.load(getMethod.getResponseBodyAsStream)
          throw new Exception(("Failed to get resource, code " + status).asInstanceOf[String])
        }
      }
    }finally{
      getMethod.releaseConnection()
    }
  }

  def getResourseContentType(path:String) : String = {
    for( node <- ((getNodeMetadata(path) \\ "metadata")(0) \\ "element")){
      if((node \ "@key") == "content.type")
        return (node \ "value").text
    }
    "text/plain" // assumed by default if no content type was found
  }


  def getNodeData(path:String):Array[Byte] = {

    val getMethod = createGetMethod(URIUtil.encodeQuery(path,"UTF-8"))

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


  def uploadFile( path:String, fileByteArray:Array[Byte], metadata:Map[String, String] = Map()) = {
    val fileBytePartSource = new ByteArrayPartSource(fileByteArray)
    writeData(path, new FilePart("data", fileBytePartSource), metadata)
  }
  def uploadFileRest( fileByteArray:Array[Byte], metadata:Map[String, String] = Map()) = {
    val fileBytePartSource = new ByteArrayPartSource(fileByteArray)
    writeDataRest(new FilePart("data", fileBytePartSource), metadata)
  }

  def updateResource(path:String, array:Array[Byte]) = {
    val fileBytePartSource = new ByteArrayPartSource(array)
    putResource(path, new FilePart("data", fileBytePartSource), Map[String, String]())
  }

  private def writeDataRest(filePart:FilePart, metadata:Map[String, String]) = {
    val postMethod = createPostMethodRest()

    val parts:Array[Part] = (filePart ::
      metadata.map(e => new StringPart(e._1, e._2, "UTF-8")).toList).toArray

    postMethod.setRequestEntity(new MultipartRequestEntity(parts, postMethod.getParams))

    try{
      val status = httpClient.executeMethod(postMethod)
      status match {
        case HttpStatus.SC_CREATED => postMethod.getResponseBodyAsString()
        case _ =>
          logger.debug("Failed to post resource. Response: " + postMethod.getResponseBodyAsString)
          throw new C3ClientException(("Filed to post resource via RestApi, code " + status).asInstanceOf[String])
      }
    }finally {
      postMethod.releaseConnection
    }
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

  private def putResourceRest(path:String, filePart:FilePart, metadata:Map[String, String]) = {
    val putMethod = createPutMethodREST()

    val parts:Array[Part] = (filePart ::
      metadata.map(e => new StringPart(e._1, e._2, "UTF-8")).toList).toArray

    putMethod.setRequestEntity(new MultipartRequestEntity(parts, putMethod.getParams))

    try{
      val status = httpClient.executeMethod(putMethod)
      status match {
        case HttpStatus.SC_OK => putMethod.getResponseBodyAsString
        case _ =>
          logger.debug("Failed to put resource. Response: " + putMethod.getResponseBodyAsString)
          throw new C3ClientException(("Filed to put resource "+ path +" , code " + status).asInstanceOf[String])
      }
    }finally {
      putMethod.releaseConnection
    }
  }
  private def createPutMethodREST():PutMethod = {
    logger.info(host + contextRestPath + "resource/")

    val method = new PutMethod(host + contextRestPath + "resource/")
    addAuthHeader(method, contextRestPath + "resource/")
    method
  }


  private def putResource(path:String, filePart:FilePart, metadata:Map[String, String]) = {
    val putMethod = createPutMethod(path)

    val parts:Array[Part] = (filePart ::
      metadata.map(e => new StringPart(e._1, e._2, "UTF-8")).toList).toArray

    putMethod.setRequestEntity(new MultipartRequestEntity(parts, putMethod.getParams))

    try{
      val status = httpClient.executeMethod(putMethod)
      status match {
        case HttpStatus.SC_OK =>
        case _ =>
          logger.debug("Failed to put resource. Response: " + putMethod.getResponseBodyAsString)
          throw new C3ClientException(("Filed to put resource "+ path +" , code " + status).asInstanceOf[String])
      }
    }finally {
      putMethod.releaseConnection
    }
  }

  def getResourceAsString(path:String):String = {
    val getMethod = createGetMethod(path)

    val status = httpClient.executeMethod(getMethod)

    status match {
      case HttpStatus.SC_OK => {
        new String(getMethod.getResponseBody, "UTF-8")
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

  def doSearch(target : String) = {

                      //  TODO URIUtil.encodeQuery(name, "UTF-8")
    val getRequest = createGetSearchMethod(URLEncoder.encode(target))

    val resultSet = {
      try{
        val status = httpClient.executeMethod(getRequest)
        status match{
          case HttpStatus.SC_OK =>
            if(isXmlResponse(getRequest))
              XML.load(getRequest.getResponseBodyAsStream)
            else{
              throw new C3ClientException("Failed to get search result set. Unexpected content type")
            }

          case _ => {
            logger.warn("Failed to get search result set, code: " + status + " response: " + getRequest.getResponseBodyAsString)
            throw new C3ClientException("Failed to do search")
          }
        }
      }finally {
        getRequest.releaseConnection
      }
    }

    (resultSet \\ "searchResults")(0)
  }

   private def createPostMethodRest():PostMethod = {
    logger.info(host + contextRestPath + "resource/")
    val method = new PostMethod(host + contextRestPath + "resource/")
    addAuthHeader(method, contextRestPath + "resource/")
    method
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
    addAuthHeader(method, contextPath + relativePath.split("\\?metadata").head)
    method
  }


  private def createGetRestMethod(relativePath : String): GetMethod = {
    logger.info(host + contextRestPath+"resource/" + relativePath)

    val method = new GetMethod(host + contextRestPath + "resource/" + relativePath)
    addAuthHeader(method, contextRestPath + "resource/" + relativePath.split("\\?metadata").head)
    method

  }




  private def createGetSearchMethod(searchString : String):GetMethod = {
    logger.info(host + contextRestSearchPath  + searchString)
                                                              //URIUtil.encodeAll(searchString)
    val method = new GetMethod(host + contextRestSearchPath + searchString)
    addAuthHeader(method, contextRestSearchPath + searchString)
    method
  }

  private def createDeleteMethod(relativePath:String):DeleteMethod = {
    logger.info(host + contextPath + relativePath)

    val method = new DeleteMethod(host + contextPath + relativePath)
    addAuthHeader(method, contextPath + relativePath)
    method
  }

  private def createPutMethod(relativePath:String):PutMethod = {
    logger.info(host + contextPath + relativePath)

    val method = new PutMethod(host + contextPath + relativePath)
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

    val contextRestPath = Props.get("c3_context_rest_path") openOr("/rest/")

    val domain = Props.get("c3_domain_name") openOr "anonymous"

    val secret = Props.get("c3_domain_secret") openOr ""

    new C3Client(host, contextPath, contextRestPath , domain, secret)
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

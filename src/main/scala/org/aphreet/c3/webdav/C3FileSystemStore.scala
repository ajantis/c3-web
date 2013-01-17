package org.aphreet.c3.webdav

import net.sf.webdav.{StoredObject, ITransaction, IWebdavStore}
import java.io.{File, InputStream}
import java.security.Principal
import org.slf4j.LoggerFactory
import org.aphreet.c3.apiaccess.C3
import com.ifunsoftware.c3.access.fs.C3FileSystemNode
import java.nio.file.{StandardCopyOption, Files}
import com.ifunsoftware.c3.access.{C3AccessException, C3ByteChannel, DataStream}
import javax.servlet.http.HttpServletRequest
import org.aphreet.c3.model.User
import net.liftweb.mapper.By
import org.apache.commons.codec.binary.Base64
import net.liftweb.common.Full
import net.sf.webdav.exceptions.{AccessDeniedException, UnauthenticatedException}

class C3FileSystemStore(val root:File) extends IWebdavStore{

  val log = LoggerFactory.getLogger(getClass)

  val c3System = C3()

  def createPrincipal(request: HttpServletRequest):Principal = {

    val authHeader = request.getHeader("Authorization")

    if(authHeader != null){
      val loginPassword = new String(Base64.decodeBase64(authHeader.replaceFirst("Basic\\s+", "") .getBytes("UTF-8")), "UTF-8")

      val credentials = loginPassword.split(":", 2)

      val mail = credentials(0)
      val password = credentials(1)

      User.find(By(User.email, mail)) match {
        case Full(user) => {
          if(user.password.match_?(password)){
            new C3Principal(user)
          }
          else throw new AccessDeniedException()
        }
        case _ => throw new AccessDeniedException()
      }

    }else{
      throw new UnauthenticatedException()
    }
  }

  def begin(principal: Principal):ITransaction = {
    log.debug("begin()")
    new C3Transaction(principal)
  }

  def checkAuthentication(tx: ITransaction) {
    if(tx.getPrincipal == null){
      throw new AccessDeniedException()
    }
  }

  def commit(tx: ITransaction) {
    log.debug("commit()")
    log.debug("Cached nodes: {}", tx.cachedFiles)

    tx.close()
  }

  def rollback(tx: ITransaction) {
    log.info("rollback()")

    tx.close()
  }

  def destroy() {
    log.info("destroy()")
  }

  def createFolder(tx: ITransaction, uri: String) {
    log.info("createFolder() " + uri)
  }

  def createResource(tx: ITransaction, uri: String) {
    log.info("createResource() " + uri)
  }

  def getResourceContent(tx: ITransaction, uri: String) = {
    getFSNode(tx, uri).asFile.versions.last.getDataStream
  }

  def setResourceContent(tx: ITransaction, uri: String, content: InputStream, contentType: String, characterEncoding: String) = {
    log.info("setResourceContent() " + uri)

    val tmpFile = File.createTempFile("dav_upload-", null).toPath

    try{
      Files.copy(content, tmpFile, StandardCopyOption.REPLACE_EXISTING)
      getFSNode(tx, uri).update(DataStream(tmpFile.toFile))
      tmpFile.toFile.length()
    }finally {
      Files.deleteIfExists(tmpFile)
    }
  }

  def getChildrenNames(tx: ITransaction, uri: String):Array[String] = {
    log.debug("getChildrenNames() {}", uri )

    val file = getFSNode(tx, uri)

    if(file.isDirectory){
      val dir = file.asDirectory
      dir.children().map(node =>
        cacheNode(tx, node.fullname, node).name
      ).toArray
    }else{
      Array()
    }
  }

  def getResourceLength(tx: ITransaction, uri: String) = {
    log.info("getResourceLength() " + uri)
    getFSNode(tx, uri).versions.last.length
  }

  def removeObject(tx: ITransaction, uri: String) {
    log.info("removeObject")
  }

  def getStoredObject(tx: ITransaction, uri: String) = {
    log.debug("getStoredObject() {}", uri)

    try{
      val file = getFSNode(tx, uri)

      val storedObject = new StoredObject
      storedObject.setFolder(file.isDirectory)
      storedObject.setMimeType(file.metadata.getOrElse("content.type", "application/octet-stream"))
      storedObject.setResourceLength(file.versions.last.length)
      storedObject.setCreationDate(file.date)
      storedObject.setLastModified(file.versions.last.date)

      storedObject
    }catch{
      case e: C3AccessException => null
    }
  }

  private def getFSNode(tx:ITransaction, uri:String):C3FileSystemNode = {
    tx.cachedFiles.get(uri.replaceAll("/+", "/")) match {
      case Some(node) => node
      case None => {
        cacheNode(tx, uri, c3System.getFile(uri))
      }
    }
  }

  private def cacheNode(tx:ITransaction, uri:String, node:C3FileSystemNode):C3FileSystemNode = {
    tx.cachedFiles.put(uri.replaceAll("/+", "/"), node)
    node
  }

  private def use(tx:ITransaction, channel:C3ByteChannel):C3ByteChannel = {
    tx.openedChannels.add(channel)
    channel
  }

  implicit def txToc3Tx(tx:ITransaction):C3Transaction = {
    tx.asInstanceOf[C3Transaction]
  }

  implicit def principalToC3Princioal(principal: Principal): C3Principal = {
    principal.asInstanceOf[C3Principal]
  }
}

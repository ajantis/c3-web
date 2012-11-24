package org.aphreet.c3.webdav

import net.sf.webdav.{StoredObject, ITransaction, IWebdavStore}
import java.io.{File, InputStream}
import java.security.Principal
import org.slf4j.LoggerFactory
import org.aphreet.c3.apiaccess.C3
import com.ifunsoftware.c3.access.fs.C3FileSystemNode

class C3FileSystemStore(val root:File) extends IWebdavStore{

  val log = LoggerFactory.getLogger(getClass)

  val c3System = C3()

  def destroy() {
    log.info("destroy()")
  }

  def begin(p1: Principal):ITransaction = {
    log.debug("begin()")
    new C3Transaction
  }

  def checkAuthentication(tx: ITransaction) {
    log.info("checkAuthentication()")
  }

  def commit(tx: ITransaction) {
    log.debug("commit()")
    log.debug("Cached nodes: {}", tx.asInstanceOf[C3Transaction].cachedFiles)
  }

  def rollback(tx: ITransaction) {
    log.info("rollback()")
  }

  def createFolder(tx: ITransaction, uri: String) {
    log.info("createFolder() " + uri)
  }

  def createResource(tx: ITransaction, uri: String) {
    log.info("createResource() " + uri)
  }

  def getResourceContent(tx: ITransaction, uri: String) = {
    log.info("getResourceContent() " + uri)
    null
  }

  def setResourceContent(tx: ITransaction, uri: String, content: InputStream, contentType: String, characterEncoding: String) = {
    log.info("setResourceContent() " + uri)
    0L
  }

  def getChildrenNames(tx: ITransaction, uri: String):Array[String] = {
    log.info("getChildrenNames() " + uri )

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
    0L
  }

  def removeObject(tx: ITransaction, uri: String) {
    log.info("removeObject")
  }

  def getStoredObject(tx: ITransaction, uri: String) = {
    log.debug("getStoredObject() {}", uri)

    val file = getFSNode(tx, uri)

    val storedObject = new StoredObject
    storedObject.setFolder(file.isDirectory)
    storedObject.setMimeType(file.systemMetadata.getOrElse("content.type", ""))
    storedObject.setResourceLength(file.versions.last.getData.length)
    storedObject.setCreationDate(file.date)
    storedObject.setLastModified(file.versions.last.date)

    storedObject
  }

  private def getFSNode(tx:ITransaction, uri:String):C3FileSystemNode = {
    val c3Tx = tx.asInstanceOf[C3Transaction]
    c3Tx.cachedFiles.get(uri.replaceAll("/+", "/")) match {
      case Some(node) => node
      case None => {
        cacheNode(tx, uri, c3System.getFile(uri))
      }
    }
  }

  private def cacheNode(tx:ITransaction, uri:String, node:C3FileSystemNode):C3FileSystemNode = {
    tx.asInstanceOf[C3Transaction].cachedFiles.put(uri.replaceAll("/+", "/"), node)
    node
  }
}

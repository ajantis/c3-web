package org.aphreet.c3.webdav

import net.sf.webdav.{ StoredObject, ITransaction, IWebdavStore }
import java.io.{ File, InputStream }
import java.security.Principal
import org.slf4j.LoggerFactory
import org.aphreet.c3.apiaccess.C3
import com.ifunsoftware.c3.access.fs.C3FileSystemNode
import java.nio.file.{ StandardCopyOption, Files }
import com.ifunsoftware.c3.access.{ MetadataValue, C3AccessException, DataStream }
import javax.servlet.http.HttpServletRequest
import org.aphreet.c3.model.User
import net.liftweb.mapper.By
import org.apache.commons.codec.binary.Base64
import net.liftweb.common._
import net.sf.webdav.exceptions.{ ObjectNotFoundException, AccessDeniedException, UnauthenticatedException }
import org.aphreet.c3.lib.metadata.Metadata
import scala.language.implicitConversions
import net.liftweb.util.Helpers._
import org.aphreet.c3.comet.{ MessageServerFactory, JournalServer }
import net.liftweb.common.Full
import scala.Some
import org.aphreet.c3.comet.JournalServerEvent
import com.ifunsoftware.c3.access.StringMetadataValue
import org.aphreet.c3.model.Group
import org.aphreet.c3.service.journal.EventType.EventType
import org.aphreet.c3.service.journal.EventType

class C3FileSystemStore(val root: File) extends IWebdavStore {

  val log = LoggerFactory.getLogger(getClass)

  val c3System = C3()

  //cash journal servers
  var cashMapJournalServers = Map[String, Box[JournalServer]]()

  def createPrincipal(request: HttpServletRequest): Principal = {

    val authHeader = request.getHeader("Authorization")

    if (authHeader != null) {
      val loginPassword = new String(Base64.decodeBase64(authHeader.replaceFirst("Basic\\s+", "").getBytes("UTF-8")), "UTF-8")

      val credentials = loginPassword.split(":", 2)

      val mail = credentials(0)
      val password = credentials(1)

      User.find(By(User.email, mail)) match {
        case Full(user) =>
          if (user.password.match_?(password)) {
            new C3Principal(user)
          } else throw new AccessDeniedException()
        case _ => throw new AccessDeniedException()
      }
    } else {
      throw new UnauthenticatedException()
    }
  }

  def begin(principal: Principal): ITransaction = {
    log.debug("begin()")
    new C3Transaction(principal)
  }

  def checkAuthentication(tx: ITransaction) {
    if (tx.getPrincipal == null) {
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
    log.debug("createFolder() " + uri)

    val splitUri = uri.split("/")
    val parent = splitUri.dropRight(1).mkString("/")
    val child = splitUri.last

    log.debug("Going to create directory \"" + child + "\" in the dir " + parent)

    val defaultMeta = getDefaultMetadata(tx, uri)

    getFSNode(tx, parent) match {
      case Full(file) =>
        file.asDirectory.createDirectory(child, defaultMeta)
        trackEvent(tx, EventType.CreateResources, translateUri(uri, tx))
      case _ => throw new ObjectNotFoundException()
    }
  }

  def createResource(tx: ITransaction, uri: String) {
    log.debug("createResource() " + uri)
    tx.createdFiles.add(uri)
  }

  def getResourceContent(tx: ITransaction, uri: String) = getFSNode(tx, uri) match {
    case Full(file) => file.asFile.versions.last.getDataStream
    case _          => throw new ObjectNotFoundException()
  }

  def setResourceContent(tx: ITransaction, uri: String, content: InputStream, contentType: String, characterEncoding: String) = {
    log.debug("setResourceContent() " + uri)

    val tmpFile = File.createTempFile("dav_upload-", null).toPath

    try {
      Files.copy(content, tmpFile, StandardCopyOption.REPLACE_EXISTING)

      if (tx.createdFiles.contains(uri)) {

        val splitUri = uri.split("/")

        val parent = splitUri.dropRight(1).mkString("/")
        val child = splitUri.last

        val defaultMeta = getDefaultMetadata(tx, uri)
        getFSNode(tx, parent) match {
          case Full(file) =>
            file.asDirectory.createFile(child, defaultMeta, DataStream(tmpFile.toFile))
            trackEvent(tx, EventType.CreateResources, translateUri(uri, tx))
          case _ => throw new ObjectNotFoundException()
        }
        tx.createdFiles.remove(uri)
      } else {
        getFSNode(tx, uri) match {
          case Full(file) => file.update(DataStream(tmpFile.toFile))
          case _          => throw new ObjectNotFoundException()
        }
      }

      tmpFile.toFile.length()
    } finally {
      Files.deleteIfExists(tmpFile)
    }
  }

  def getChildrenNames(tx: ITransaction, uri: String): Array[String] = {
    log.debug("getChildrenNames() {}", uri)

    getFSNode(tx, uri) match {
      case Full(file) =>
        if (file.isDirectory) {
          val dir = file.asDirectory

          val groupFilter = if (uri == "/") {
            (name: String) => tx.getPrincipal.groups.contains(name)
          } else {
            (name: String) => true
          }

          dir.children().map(node =>
            cacheNode(tx, node.fullname, node).name).filter(groupFilter).toArray
        } else {
          Array()
        }
      case _ => throw new ObjectNotFoundException()
    }
  }

  def getResourceLength(tx: ITransaction, uri: String) = {
    log.info("getResourceLength() " + uri)
    getFSNode(tx, uri) match {
      case Full(file) => file.versions.last.length
      case _          => throw new ObjectNotFoundException()
    }
  }

  def removeObject(tx: ITransaction, uri: String) {
    log.info("removeObject " + uri)
    deleteFromCache(tx, uri)
    c3System.deleteFile(translateUri(uri, tx))
  }

  def getStoredObject(tx: ITransaction, uri: String): StoredObject = {
    log.debug("getStoredObject() {}", uri)

    getFSNode(tx, uri) match {
      case Full(file) =>
        val storedObject = new StoredObject
        storedObject.setFolder(file.isDirectory)
        storedObject.setMimeType(file.metadata.getOrElse("content.type", "application/octet-stream"))
        storedObject.setResourceLength(file.versions.last.length)
        storedObject.setCreationDate(file.date)
        storedObject.setLastModified(file.versions.last.date)

        storedObject
      case Empty                                        => new StoredObject
      case Failure(_, e: C3AccessException, _)          => throw new AccessDeniedException()
      case Failure(_, e: GroupAccessDeniedException, _) => throw new AccessDeniedException()
      case Failure(_, _, _)                             => null
    }
  }

  private def getFSNode(tx: ITransaction, uri: String): Box[C3FileSystemNode] = {

    val translatedUri = translateUri(uri.replaceAll("/+", "/"), tx)

    tx.cachedFiles.get(translatedUri) match {
      case Some(node) => Full(node)
      case None =>
        tryo(c3System.getFile(translatedUri)) match {
          case Full(node) => Full(cacheNode(tx, uri, node))
          case any =>
            if (tx.createdFiles.contains(uri)) Empty
            else any
        }
    }
  }

  private def translateUri(uri: String, tx: ITransaction): String = {

    val splitUri = uri.split("/", 3).filter(!_.isEmpty)

    val translatedUri = splitUri.length match {
      case 0 => "/"
      case 1 => "/" + verifyGroupAccess(splitUri(0), tx) + "/files"
      case _ => "/" + verifyGroupAccess(splitUri(0), tx) + "/files/" + splitUri(1)
    }

    if (log.isDebugEnabled) {
      log.debug("URI " + uri + " translated to " + translatedUri)
    }
    translatedUri
  }

  private def verifyGroupAccess(groupId: String, tx: ITransaction): String = {
    if (!tx.getPrincipal.groups.contains(groupId)) {
      log.debug("Access denied to group " + groupId + " for principal " + tx.getPrincipal.getName)
      throw new GroupAccessDeniedException
    } else
      groupId
  }

  private def deleteFromCache(tx: ITransaction, uri: String) = {
    tx.cachedFiles.remove(uri.replaceAll("/+", "/"))
  }

  private def cacheNode(tx: ITransaction, uri: String, node: C3FileSystemNode): C3FileSystemNode = {
    tx.cachedFiles.put(uri.replaceAll("/+", "/"), node)
    node
  }

  implicit def txToc3Tx(tx: ITransaction): C3Transaction = {
    tx.asInstanceOf[C3Transaction]
  }

  implicit def principalToC3Principal(principal: Principal): C3Principal = {
    principal.asInstanceOf[C3Principal]
  }

  def supportsMoveOperation() = true

  def moveResource(tx: ITransaction, source: String, destination: String) {
    log.debug("Called move from " + source + " to " + destination)
    getFSNode(tx, source) match {
      case Full(file) =>
        val translUri = translateUri(destination, tx)
        file.move(translUri)
        trackEvent(tx, EventType.MoveResources, translUri)
      case _ => throw new ObjectNotFoundException()
    }
  }

  def getDefaultMetadata(tx: ITransaction, uri: String): Map[String, MetadataValue] = {
    val splitUri = uri.split("/", 3).filter(!_.isEmpty)

    Map(Metadata.GROUP_ID_META -> StringMetadataValue(splitUri(0)),
      Metadata.OWNER_ID_META -> StringMetadataValue(tx.getPrincipal.getUser.id.toString()))
  }

  def trackEvent(tx: ITransaction, event: EventType, path: String) {
    val group = getGroupFromURI(path)
    val journalServer: Box[JournalServer] = getJournalServer(group)
    journalServer.foreach(_ ! JournalServerEvent(tx.getPrincipal.getUser, group, event, path))
  }

  def getJournalServer(group: Group): Box[JournalServer] = {
    val cashedJournalServer = cashMapJournalServers.get(group.name.toString())
    cashedJournalServer match {
      case Some(journalServer) => journalServer
      case None =>
        val journalServer: Box[JournalServer] = Box(MessageServerFactory(group))
        cashMapJournalServers += (group.name.toString() -> journalServer)
        journalServer
    }
  }

  private def getGroupFromURI(uri: String): Group = {
    val groupId = uri.split("/", 3).filter(!_.isEmpty)(0)
    Group.find(groupId) match {
      case Full(group) => group
      case _           => throw new ObjectNotFoundException()
    }
  }

  class GroupAccessDeniedException extends Exception
}

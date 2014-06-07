package org.aphreet.c3.snippet.groups.snippet

import net.liftweb.http.js.{JsCmds, JsCmd}
import java.util.Calendar
import com.ifunsoftware.c3.access.fs.{C3File, C3FileSystemNode}
import com.ifunsoftware.c3.access.{C3System, MetadataUpdate}
import org.aphreet.c3.lib.metadata.Metadata
import Metadata._
import net.liftweb.http.S
import net.liftweb.util._


/**
 * @author a-legotin
 */
object FileSharingHelper {

  def shareFile(node: C3FileSystemNode): JsCmd = {
    try {

      val timestamp = Calendar.getInstance().getTimeInMillis
      val hash = md5Hash(node.name + timestamp)
      node.update(MetadataUpdate((Map(HASH -> C3System.stringToMetadataValue(hash)))))
      JsCmds.SetValById("txtHash",fileShareFullUrl(node.asFile))

    } catch {
      case e: Exception => JsCmds.Alert("Failed to sharing file")
    }
  }

  def disableSharing(node: C3FileSystemNode): JsCmd = {
    try {
      node.update(MetadataUpdate((Map(HASH -> C3System.stringToMetadataValue("")))))
      JsCmds.SetValById("txtHash","")

    } catch {
      case e: Exception => JsCmds.Alert("Failed removing shared link")
    }
  }

  /**
   * @author a-legotin
   * @define Generate MD5 hash for link building
   * @usecase md5Hash("Name")
   * @param text wich need to generate md5 hash
   * @return genereted md5hash
   */
  def md5Hash(text: String) : String = java.security.MessageDigest.getInstance("MD5").digest(text.getBytes()).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}

  def fileSharePath(file: C3File, hash: String): String = {
    var splittedFullPath = file.fullname.split("/")
    val name = Helpers.urlEncode(splittedFullPath.last)
    val path = splittedFullPath.dropRight(splittedFullPath.indexOf(splittedFullPath.last) - 2 ).mkString("/")
    s"/sharing$path/$name?id=$hash"
  }

  def fileShareFullUrl(file: C3File): String = {
    val scheme = S.request.map(_.request.scheme).openOr("")
    val server = S.request.map(_.request.serverName).openOr("")
    val port = S.request.map(_.request.serverPort).openOr("")
    val host = s"$scheme://$server:$port"
    val sharingUrl = fileSharePath(file, file.metadata.get(HASH).getOrElse(""))
    s"$host$sharingUrl"
  }
}

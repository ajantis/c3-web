package org.aphreet.c3.snippet.groups.snippet

import org.aphreet.c3.model.{User, Group}
import com.ifunsoftware.c3.access.fs.{C3FileSystemNode, C3File}
import net.liftweb.common.{Empty, Box}
import org.aphreet.c3.lib.metadata.Metadata._
import scala.Some
import net.liftweb.mapper.By

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
trait C3ResourceHelpers {

  val group: Group

  def fileDownloadUrl(file: C3File): String = "/download" + file.fullname + "?dl=true"

  def fileViewUrl(file: C3File): String = "/download" + file.fullname

  def nodeOwner(node: C3FileSystemNode): Box[User] = node.metadata.get(OWNER_ID_META) match {
    case Some(id) if !id.isEmpty => User.find(By(User.id, id.toLong))
    case _ => Empty
  }
}
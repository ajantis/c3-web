package org.aphreet.c3.util.helpers

import com.ifunsoftware.c3.access.fs.{ C3File, C3FileSystemNode }
import net.liftweb.common.{ Box, Empty }
import net.liftweb.mapper.By
import org.aphreet.c3.lib.metadata.Metadata._
import org.aphreet.c3.model.{ Group, User }

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
    case _                       => Empty
  }
}
package org.aphreet.c3.snippet.group

import org.aphreet.c3.model.Group

/**
 * @author Dmitry Ivanov (mailto: Dmitry.Ivanov@reltio.com)
 *         Reltio, Inc.
 */
case class GroupPageFilesData(override val group: Group, private val _path: List[String]) extends GroupPageData(group){
  val isDirectoryLoc = _path.lastOption.map(_ == "index").getOrElse(true)
  val path = _path.lastOption match {
    case Some("index") => {
      _path.dropRight(1)
    }
    case _ => _path
  }

  val currentAddress = if(!path.isEmpty) path.mkString("/","/", if(isDirectoryLoc)"/" else "") else "/"
}

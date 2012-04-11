package org.aphreet.c3.service.impl

import org.aphreet.c3.service.GroupService
import com.ifunsoftware.c3.access.C3System
import org.aphreet.c3.lib.DependencyFactory._

class GroupServiceImpl extends GroupService{

  lazy val c3 = inject[C3System].open_!

  def createGroupMapping(name: String){
    val root = c3.getFile("/").asDirectory

    root.createDirectory(name)

    root.getChild(name) match {
      case Some(node) => val dir = node.asDirectory
      dir.createDirectory("files")
      dir.createDirectory("wiki")
      case None =>
    }
  }
}

object GroupServiceImpl{
  
  def create:GroupService = new GroupServiceImpl
  
}

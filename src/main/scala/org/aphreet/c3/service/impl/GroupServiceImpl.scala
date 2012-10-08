package org.aphreet.c3.service.impl

import org.aphreet.c3.service.GroupService
import com.ifunsoftware.c3.access.C3System
import org.aphreet.c3.lib.DependencyFactory._
import com.ifunsoftware.c3.access.fs.{C3FileSystemNode, C3File, C3Directory}
import annotation.tailrec
import net.liftweb.common.{Box, Full}
import org.aphreet.c3.util.C3Exception

class GroupServiceImpl extends GroupService{

  lazy val c3 = inject[C3System].open_!

  def createGroupMapping(name: String){
    val root = c3.getFile("/").asDirectory

    root.createDirectory(name)

    root.getChild(name) match {
      case Some(node) => val dir = node.asDirectory
      dir.createDirectory("files")
      dir.createDirectory("messages")
      dir.createDirectory("wiki")
      case None =>
    }
  }

  def removeGroupMapping(name: String){
    val root = c3.getFile("/").asDirectory

    def removeDirectory(dir: C3Directory) {
      for(child <- dir.children()){
        child match {
          case d: C3Directory => {
            removeDirectory(d)
          }
          case f: C3File => {
            System.out.println(f.fullname)
            c3.deleteFile(f.fullname)
          }
        }
      }
      c3.deleteFile(dir.fullname)
    }

    root.getChild(name).filter(_.isDirectory) match {
      case Some(groupDir) => {
        removeDirectory(groupDir.asDirectory)
      }
      case _ =>
        throw new C3Exception("Group directory is not found!")
    }
  }
}

object GroupServiceImpl{
  def create: GroupService = new GroupServiceImpl
}


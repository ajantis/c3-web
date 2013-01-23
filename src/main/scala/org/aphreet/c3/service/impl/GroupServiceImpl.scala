package org.aphreet.c3.service.impl

import org.aphreet.c3.service.{AddedToGroupMsg, CreateNotification, NotificationManager, GroupService}
import com.ifunsoftware.c3.access.C3System
import org.aphreet.c3.lib.DependencyFactory._
import com.ifunsoftware.c3.access.fs.{C3File, C3Directory}
import org.aphreet.c3.util.C3Exception
import org.aphreet.c3.model.{UserGroup, Group, User}

class GroupServiceImpl extends GroupService{

  lazy val c3 = inject[C3System].open_!

  def createGroupMapping(groupId: String){
    val root = c3.getFile("/").asDirectory

    root.createDirectory(groupId)

    root.getChild(groupId) match {
      case Some(node) => val dir = node.asDirectory
      dir.createDirectory("files")
      dir.createDirectory("messages")
      dir.createDirectory("wiki")
      case None => throw new C3Exception("Failed to create directory for group " + groupId)
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

  def createGroup(newGroup: Group, members: Iterable[User]): Group = {
    val group = newGroup.saveMe()
    try {
      createGroupMapping(group.id.is.toString)
    } catch {
      case e: C3Exception => {
        group.delete_! // rollback all changes
        throw e
      }
    }
    addUserGroup(group,members)
    group
  }

  def addUserGroup(currentGroup:Group, members: Iterable[User]){
    currentGroup.owner.foreach(owner => UserGroup.join(owner, currentGroup))
    for {
      member <- members
    } {
      UserGroup.join(member, currentGroup)
      NotificationManager ! CreateNotification(AddedToGroupMsg(group = currentGroup, recipient = member))
    }
  }
}

object GroupServiceImpl{
  def create: GroupService = new GroupServiceImpl
}


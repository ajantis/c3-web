package org.aphreet.c3.util.helpers

import com.ifunsoftware.c3.access.fs.C3FileSystemNode
import net.liftweb.common.Full
import org.aphreet.c3.acl.resources.C3FileAccess
import org.aphreet.c3.lib.metadata.Metadata._
import org.aphreet.c3.model.{Group, User}

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */

//[TODO] need correct with opportunity approve user in group
trait C3AccessHelpers extends C3FileAccess with C3ResourceHelpers {

  def hasWriteAccess(group: Group) = {
    User.currentUser match {
      case Full(user) => !user.groups.filter(_ == group).isEmpty
      case _ => false
    }
  }

  def hasSuperAccess = {
    User.currentUser match {
      case Full(user) => user.superUser.is
      case _          => false
    }
  }

  def checkReadAccessResource(resource: C3FileSystemNode) = {
    if (User.containsCurrent(group.users.toList))
      isGroupRead(resource)
    else
      isOtherUserRead(resource)
  }

  def isGroupRead(resource: C3FileSystemNode) = {
    val metaACL = acl(resource.metadata.get(ACL_META).getOrElse(""))
    metaACL.charAt(0) == 'r'
  }

  def isOtherUserRead(resource: C3FileSystemNode) = {
    val metaACL = acl(resource.metadata.get(ACL_META).getOrElse(""))
    metaACL.charAt(2) == 'r'
  }

  def hasWriteAccessResource(resource: C3FileSystemNode) = {
    if (User.containsCurrent(group.users.toList))
      isGroupWrite(resource)
    else
      isOtherUserWrite(resource)
  }

  def isGroupWrite(resource: C3FileSystemNode) = {
    val metaACL = acl(resource.metadata.get(ACL_META).getOrElse(""))
    metaACL.charAt(1) == 'w'
  }

  def isOtherUserWrite(resource: C3FileSystemNode) = {
    val metaACL = acl(resource.metadata.get(ACL_META).getOrElse(""))
    metaACL.charAt(3) == 'w'
  }

  //default value acl
  def acl(acl: String) = {
    if (acl == "")
      if (group.isOpen.is)
        "r-r-"
      else
        "r---"
    else
      acl
  }

  def hasSuperAccessResource(resource: C3FileSystemNode) = {
    val owner = nodeOwner(resource)
    User.currentUser match {
      case Full(user) => user.superUser.is || User.containsCurrent(owner.toList)
      case _          => false
    }
  }
}


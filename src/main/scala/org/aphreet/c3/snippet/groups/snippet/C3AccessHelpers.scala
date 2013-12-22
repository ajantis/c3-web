package org.aphreet.c3.snippet.groups.snippet

import com.ifunsoftware.c3.access.fs.C3FileSystemNode
import org.aphreet.c3.lib.metadata.Metadata._
import org.aphreet.c3.model.User
import net.liftweb.common.Full
import org.aphreet.c3.model.Group

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
trait C3AccessHelpers extends C3FileAccess with C3ResourceHelpers{

  //default value acl
  def acl(acl:String) = {
    if(acl == "")
      if(group.isOpen.is)
        "r-r-"
      else
        "r---"
    else
      acl
  }

  def hasWriteAccess(group:Group) = {
    User.currentUser match {
      case Full(user) => !user.groups.filter(_==group).isEmpty
      case _ => false
    }
  }

  def hasSuperAccess = {
    User.currentUser match {
      case Full(user) => user.superUser.is
      case _ => false
    }
  }

  def isGroupRead(resource:C3FileSystemNode) = {
    val metaACL = acl(resource.metadata.get(ACL_META).getOrElse(""))
    metaACL.charAt(0) == 'r'
  }

  def isGroupWrite(resource:C3FileSystemNode) = {
    val metaACL = acl(resource.metadata.get(ACL_META).getOrElse(""))
    metaACL.charAt(1) == 'w'
  }

  def isOtherUserRead(resource:C3FileSystemNode) = {
    val metaACL = acl(resource.metadata.get(ACL_META).getOrElse(""))
    metaACL.charAt(2) == 'r'
  }

  def isOtherUserWrite(resource:C3FileSystemNode) = {
    val metaACL = acl(resource.metadata.get(ACL_META).getOrElse(""))
    metaACL.charAt(3) == 'w'
  }

  def checkReadAccessResource(resource:C3FileSystemNode) = {
    if(User.containsCurrent(group.users.toList))
      isGroupRead(resource)
    else
      isOtherUserRead(resource)
  }

  def hasWriteAccessResource(resource:C3FileSystemNode) = {
    if(User.containsCurrent(group.users.toList))
      isGroupWrite(resource)
    else
      isOtherUserWrite(resource)
  }

  def hasSuperAccessResource(resource:C3FileSystemNode) =  {
    val owner = nodeOwner(resource)
    User.currentUser match {
      case Full(user) => user.superUser.is || User.containsCurrent(owner.toList)
      case _ => false
    }
  }
}


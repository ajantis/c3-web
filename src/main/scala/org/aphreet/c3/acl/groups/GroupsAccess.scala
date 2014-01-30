package org.aphreet.c3.acl.groups

import org.aphreet.c3.model.{UserGroup, Group, User}
import net.liftweb.common.{Empty, Full}
import net.liftweb.mapper.By


trait GroupsAccess {

  def checkAccess(user:User,group:Group) = {

    val members = UserGroup.findAll(By(UserGroup.group,group))

    val approvedMembers = members.filter(_.isApproved)
      .map(_.user.obj.openOrThrowException("Error open user"))

    lazy val otherMembers = members.filter(_.isApproved!=true)
      .map(_.user.obj.openOrThrowException("Error open user"))

    def checkMember() = {
      if(approvedMembers.contains(user))
        UserStatusGroup.Member
      else if(otherMembers.contains(user))
        UserStatusGroup.Request
      else UserStatusGroup.Other
    }

    if (user.superUser.is) UserStatusGroup.Admin else {
      group.owner.obj match {
        case Full(own) => if (own.id == user.id) UserStatusGroup.Owner else
          checkMember()
        case Empty => checkMember()
      }
    }
  }
}
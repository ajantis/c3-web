package org.aphreet.c3.acl.groups


object UserStatusGroup extends Enumeration {

  type status = Value
  val Admin, Owner, Member, Request, Other  = Value

}
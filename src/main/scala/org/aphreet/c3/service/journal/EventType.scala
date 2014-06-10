package org.aphreet.c3.service.journal

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
object EventType extends Enumeration {
  type EventType = Value

  val CreateResources = Value("create_resource")
  val UpdateResources = Value("update_resource")
  val MoveResources = Value("move_resource")
  val ApproveUserToGroup = Value("approve_user_to_group")
}

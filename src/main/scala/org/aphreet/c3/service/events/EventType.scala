package org.aphreet.c3.service.events

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
object EventType extends Enumeration {
  type EventType = Value

  val CreateResources = Value("create_resource")
  val UpdateResources = Value("update_resource")
  val MoveResources = Value("move_resource")
  val ApproveUser = Value("approve_user")
}

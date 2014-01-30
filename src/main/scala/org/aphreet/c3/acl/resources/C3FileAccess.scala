package org.aphreet.c3.acl.resources

import com.ifunsoftware.c3.access.fs.C3FileSystemNode

/**
  * @author Koyushev Sergey (mailto: serjk91@gmail.com)
*/
trait C3FileAccess {

  def checkReadAccessResource(resource:C3FileSystemNode): Boolean

  def hasWriteAccessResource(resource:C3FileSystemNode): Boolean

  def hasSuperAccessResource(resource:C3FileSystemNode): Boolean

}

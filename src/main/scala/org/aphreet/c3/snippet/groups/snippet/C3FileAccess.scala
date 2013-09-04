package org.aphreet.c3.snippet.groups.snippet

import com.ifunsoftware.c3.access.fs.C3FileSystemNode

/**
  * @author Koyushev Sergey (mailto: serjk91@gmail.com)
*/
trait C3FileAccess {

  def checkReadAccess(resource:C3FileSystemNode): Boolean

  def hasWriteAccess(resource:C3FileSystemNode): Boolean

  def hasSuperAccess(resource:C3FileSystemNode): Boolean

}

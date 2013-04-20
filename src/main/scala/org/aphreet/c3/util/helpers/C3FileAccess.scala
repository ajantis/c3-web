package org.aphreet.c3.util.helpers

import com.ifunsoftware.c3.access.fs.C3FileSystemNode

/**
  * @author Koyushev Sergey (mailto: serjk91@gmail.com)
*/
trait C3FileAccess {

  def checkReadAccess(resource:C3FileSystemNode): Boolean

  def checkWriteAccess(resource:C3FileSystemNode): Boolean

  def checkSuperAccess(resource:C3FileSystemNode): Boolean

}

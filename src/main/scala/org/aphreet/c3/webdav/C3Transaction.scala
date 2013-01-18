package org.aphreet.c3.webdav

import net.sf.webdav.ITransaction
import collection.mutable
import com.ifunsoftware.c3.access.fs.C3FileSystemNode
import collection.mutable.ArrayBuffer
import com.ifunsoftware.c3.access.C3ByteChannel
import java.security.Principal
import org.aphreet.c3.model.User

class C3Transaction(val principal:Principal) extends ITransaction{

  def getPrincipal = principal

  val cachedFiles = new mutable.HashMap[String, C3FileSystemNode]

  val openedChannels = new mutable.HashSet[C3ByteChannel]

  def close() {
    openedChannels.foreach(_.close())
  }
}

class C3Principal(val user: User) extends Principal{

  def getName = user.email
}

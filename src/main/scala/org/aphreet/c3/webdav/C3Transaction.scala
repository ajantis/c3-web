package org.aphreet.c3.webdav

import net.sf.webdav.ITransaction
import collection.mutable
import com.ifunsoftware.c3.access.fs.C3FileSystemNode

class C3Transaction extends ITransaction{

  def getPrincipal = null

  val cachedFiles = new mutable.HashMap[String, C3FileSystemNode]

}

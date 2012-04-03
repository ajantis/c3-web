package org.aphreet.c3.apiaccess

import net.liftweb.util.Props
import com.ifunsoftware.c3.access.{C3SystemFactory, C3System}

object C3 {

  def apply():C3System = {
    val host = Props.get("c3_host") openOr("http://localhost:7373")

    val domain = Props.get("c3_domain_name") openOr "anonymous"

    val secret = Props.get("c3_domain_secret") openOr ""

    new C3SystemFactory().createSystem(host, domain, secret)
  }
  
  def createGroupMapping(name:String){
    
    val system = C3()
    
    val root = system.getFile("/").asDirectory
    
    root.createDirectory(name)

    root.getChild(name) match {
      case Some(node) => val dir = node.asDirectory
        dir.createDirectory("files")
        dir.createDirectory("wiki")
      case None =>
    }

  }
}
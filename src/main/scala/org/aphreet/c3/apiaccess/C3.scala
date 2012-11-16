package org.aphreet.c3.apiaccess

import net.liftweb.util.Props
import com.ifunsoftware.c3.access.{C3SystemFactory, C3System}
import net.liftweb.common.Logger

object C3 {

  private val log = Logger("C3")

  var bundleContext:AnyRef = null

  def apply():C3System = {

    val domain = Props.get("c3_domain_name") openOr "anonymous"

//    if (bundleContext != null){
//      log.info("Found bundle context, trying to obtain local C3System instance")
//
//      new C3SystemFactory().createLocalSystem(domain, bundleContext)
//
//    }else{

      val host = Props.get("c3_host") openOr("http://localhost:7373")

      val secret = Props.get("c3_domain_secret") openOr ""


      val proxy = System.getenv("HTTP_PROXY")

      if (proxy != null){
        val hostAndPort = proxy.replaceFirst("^http://", "").split(":", 2)

        val proxyHost = hostAndPort(0)
        val proxyPort = hostAndPort(1).toInt

        new C3SystemFactory().createSystem(host, domain ,secret, 100, proxyHost, proxyPort)
      }else{
        new C3SystemFactory().createSystem(host, domain, secret)
      }
//    }

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
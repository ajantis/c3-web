package org.aphreet.c3.service.impl

import org.aphreet.c3.service.WikiService
import com.ifunsoftware.c3.access.C3System
import org.apache.commons.httpclient.util.URIUtil
import com.ifunsoftware.c3.access.fs.C3Directory
import com.ifunsoftware.c3.access.DataStream
import net.liftweb.common.Logger
import org.aphreet.c3.model.Wiki
import org.aphreet.c3.lib.DependencyFactory.inject


class WikiServiceImpl extends WikiService{

  val c3 = inject[C3System].open_!

  val logger = Logger(classOf[Wiki])

  private def encodeName(name:String):String = {
    URIUtil.encodeQuery(name, "UTF-8")
  }

  private def pageLocation(group:String, name:String) = {
    pageDirectory(group) + encodeName(name)
  }

  private def pageDirectory(group:String):String = {
    "/" + group + "/wiki/"
  }

  def getPage(group:String, name:String):Option[Wiki] = {
    try{

      val file = c3.getFile(pageLocation(group, name))

      val content = file.versions.last.getData.readContentAsString
      Some(new Wiki(name, content, file.metadata))
    }catch{
      case e => None
    }
  }

  def createPage(group:String, page:Wiki) {
    try{

      c3.getFile(pageDirectory(group)).asInstanceOf[C3Directory]
        .createFile(page.name, Map("content.type" -> "application/x-c3-wiki"), DataStream(page.content))
    }catch{
      case e => logger.warn("Failed to save resource", e)
    }
  }

  def savePage(group:String, page:Wiki) {
    try{
      c3.getFile(pageLocation(group, page.name)).update(DataStream(page.content))
    }catch{
      case e => logger.warn("Failed to save resource", e)
    }
  }

  def getMetadata(group:String, name:String):Map[String, String] = {
    try{
      c3.getFile(pageLocation(group, name)).metadata
    }catch{
      case e => {
        e.printStackTrace()
        Map()
      }
    }
  }
}

object WikiServiceImpl{

  def create():WikiService = new WikiServiceImpl

}

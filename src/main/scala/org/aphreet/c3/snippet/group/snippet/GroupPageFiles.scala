package org.aphreet.c3.snippet.group.snippet

import org.aphreet.c3.snippet.group.{GroupPageFilesData, GroupPageData}
import org.aphreet.c3.loc.{ItemRewriteLoc, SuffixLoc}
import org.aphreet.c3.model.{File, Catalog, C3Resource, Group}
import net.liftweb.common._
import net.liftweb.sitemap.Loc.Link
import xml.{NodeSeq, Text}
import net.liftweb.util.BindHelpers._
import org.aphreet.c3.lib.DependencyFactory._
import com.ifunsoftware.c3.access.C3System
import net.liftweb.http.S
import net.liftweb.common.Full
import org.aphreet.c3.snippet.group.GroupPageFilesData
import org.aphreet.c3.snippet.{FileUploadDialog, CreateDirectoryDialog}

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
object GroupPageFiles extends ItemRewriteLoc[Group, GroupPageFilesData] with SuffixLoc {

  override val name = "Files"
  override val pathPrefix = "groups" :: Nil
  override val pathSuffix = "files" ::  Nil
  override def getItem(id: String) = Group.find(id)

  // we don't use it here
  override def wrapItem(groupBox: Box[Group]) = Empty
  def wrapItemWithPath(groupBox: Box[Group], path: List[String]) = groupBox.map(GroupPageFilesData(_, path))

  override def link = {
    new Link[GroupPageFilesData](pathPrefix ++ pathSuffix){
      override def pathList(value: GroupPageFilesData): List[String] = pathPrefix ::: value.group.id.is.toString :: Nil ::: pathSuffix ::: value.path
    }
  }

  override def finishPath(itemBox: => Box[Group],
                 suffix: List[String]): Box[GroupPageFilesData] = {
    if (suffix.startsWith(pathSuffix)) wrapItemWithPath(itemBox, suffix.diff(pathSuffix)) else Empty
  }
}

class GroupPageFiles(data: GroupPageFilesData) extends C3ResourceHelpers with GroupPageHelpers{

  private val logger = Logger(classOf[GroupPageFiles])

  override lazy val activeLocId = "files"
  override lazy val group = data.group
  lazy val path = data.path

  lazy val fileUpload = new GroupFileUpload(group, data.currentAddress)

  def render = {
    ".current_path *" #> Text(data.currentAddress) &
    ".fileUploadForm *" #> ((n: NodeSeq) => fileUpload.fileUploadForm(n)) &
    (if(data.isDirectoryLoc)
      renderDirectoryLoc
    else
      renderFileLoc)
  }

  protected def renderDirectoryLoc = {
    ".child *" #> group.getChildren(data.currentAddress).map {
      resource => {
        logger.error(resource.name)
        logger.error(resource.resourceType)
        resource match {
          case c: Catalog => toCss(c)
          case f: File => toCss(f)
        }
      }
    }
  }

  protected def renderFileLoc = {
    val file = group.getFile(data.currentAddress)
    file match {
      case Full(f) => {
        toCss(f) &
        ".tag *" #> f.tags
      }
      case Empty => {
        S.warning("File not found!")
        "* *" #> NodeSeq.Empty
      }
      case Failure(msg, t, chain) => {
        S.error("Oops. something bad happen: " + msg)
        "* *" #> NodeSeq.Empty
      }
    }
  }

}

trait C3ResourceHelpers {
  import net.liftweb.util.TimeHelpers._

  def toCss(directory: Catalog) = {
    ".name *" #> directory.name &
    ".icon [class+]" #> "icon-folder-close" &
    ".created_date *" #> internetDateFormatter.format(directory.created)
  }

  def toCss(file: File) = {
    ".name *" #> file.name &
    ".icon [class+]" #> "icon-file" &
    ".created_date *" #> internetDateFormatter.format(file.created)
  }
}
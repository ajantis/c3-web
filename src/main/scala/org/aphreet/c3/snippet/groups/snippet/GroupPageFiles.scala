package org.aphreet.c3.snippet.groups.snippet

import org.aphreet.c3.loc.{ItemRewriteLoc, SuffixLoc}
import org.aphreet.c3.model.{User, Group}
import net.liftweb.common._
import net.liftweb.sitemap.Loc.{Hidden, LinkText, Link}
import xml.{NodeSeq, Text}
import net.liftweb.util.Helpers._
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full
import org.aphreet.c3.snippet.groups.GroupPageFilesData
import org.aphreet.c3.snippet.{Breadcrumbs, CreateDirectoryDialog}
import com.ifunsoftware.c3.access.fs.{C3File, C3Directory}
import org.aphreet.c3.lib.metadata.Metadata
import Metadata._
import net.liftweb.util.{CssSel, PassThru}
import net.liftweb.sitemap.{Menu, SiteMap, Loc}
import annotation.tailrec
import net.liftweb.mapper.By


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
                          restPath: List[String],
                          suffix: String = ""): Box[GroupPageFilesData] = {

    val resultPath = if(suffix.isEmpty) restPath.diff(pathSuffix) else restPath.diff(pathSuffix).init ::: List(restPath.diff(pathSuffix).last + "." + suffix)
    if (restPath.startsWith(pathSuffix)) wrapItemWithPath(itemBox, resultPath) else Empty
  }
}

class GroupPageFiles(data: GroupPageFilesData) extends C3ResourceHelpers with GroupPageHelpers with FSHelpers{

  private val logger = Logger(classOf[GroupPageFiles])

  override lazy val activeLocId = "files"
  override lazy val group = data.group
  lazy val path = data.path

  def render = {
    val file = group.getFile(data.currentAddress)

    val pathLocs = buildPathLocs

    ".base_files_path *" #> (
      ".link [href]" #> (group.createLink + "/files/") &
      ".link *" #> group.name.is
    ) &
    ".bcrumb *" #> (pathLocs.map{ (loc: Loc[_]) =>
      (if(isLocCurrent(pathLocs, loc)){
        ".link" #> <strong>{loc.title}</strong>
      } else {
        ".link [href]" #> loc.createDefaultLink &
        ".link *" #> loc.title
      }) &
      ".divider" #> (
        data.isDirectoryLoc match {
          case false if loc == pathLocs.last => (_:NodeSeq) => NodeSeq.Empty // if it is a file then we want to skip last "/" divider
          case _ => PassThru
        }
      )
    }) &
    ".current_path *" #> Text(data.currentAddress) &
    //".back_btn [href]" #> Text(data.currentAddress) &
    (file match {
      case Empty => S.redirectTo("/404.html"); "* *" #> PassThru
      case Failure(msg, t, chain) => {
        logger.error("Error accessing file: " + msg, t)
        S.redirectTo("/404.html")
        "* *" #> PassThru
      }
      case Full(f: C3File) => renderFileLoc(f)
      case Full(d: C3Directory) => {
        if(!S.uri.endsWith("/"))
          S.redirectTo(S.uri + "/")
        else
          renderDirectoryLoc(d)
      }
      case _ => "* *" #> PassThru
    })
  }

  protected def renderDirectoryLoc(d: C3Directory): CssSel = {
    ".child *" #> group.getChildren(data.currentAddress).map {
      resource => {
        resource match {
          case c: C3File => toCss(c)
          case f: C3Directory => toCss(f)
        }
      }
    } &
    ".file-view" #> NodeSeq.Empty &
    "#new-directory" #> newDirectoryForm(d, currentPath = data.group.createLink + "/files" + data.currentAddress)
  }

  protected def renderFileLoc(f: C3File): CssSel = {
    ".label_file" #> f.metadata.get(TAGS_META).map(_.split(TAGS_SEPARATOR).toList).getOrElse(Nil).map((tag: String) => {
      ".label *" #> tag
    }) &
      ".file-table" #> NodeSeq.Empty &
      ".fs_toolbar" #> NodeSeq.Empty &
      "#upload_form" #> NodeSeq.Empty &
      ".name_file *" #> f.name &
      ".download_btn [href]" #> fileDownloadUrl(f) &
      ".view_btn [href]" #> fileViewUrl(f) &
      ".data_file *" #> internetDateFormatter.format(f.date)
  }

  protected def newDirectoryForm(currentDirectory: C3Directory, currentPath: String): CssSel = {
    var name = ""
    var tags = ""

    def createDirectory(){
      // TODO send metadata for new dir to c3 (need accesslib support)
      val metadata = Map((OWNER_ID_META -> User.currentUser.map(_.id.is.toString).open_!),
        (GROUP_ID_META -> data.group.id.is),
        (TAGS_META -> tags.trim))
      currentDirectory.createDirectory(name.trim)
      S.redirectTo(currentPath) // redirect on the same page
    }

    "name=name" #> SHtml.onSubmit(name = _) &
    "name=tags" #> SHtml.onSubmit(tags = _) &
    "type=submit" #> SHtml.onSubmitUnit(createDirectory)
  }

  def buildPathLocs: List[Loc[_]] = {
    val locs: List[Loc[_]] = (transformToPathLists(data.path)).map { thisPath =>

      new Loc[List[String]] {
        private val __sitemap = SiteMap.build(Array(Menu(this)))

        override def name: String = thisPath.lastOption.getOrElse("N/A")

        override def link: Loc.Link[List[String]] = new Link[List[String]](List("groups", data.group.id.is.toString, "files") ::: thisPath)

        override def text: Loc.LinkText[List[String]] = new LinkText[List[String]](v => Text(v.lastOption.getOrElse("N/A")))

        override def defaultValue: Box[List[String]] = Full(thisPath)

        override def params = Hidden :: Nil

        override def siteMap: SiteMap = __sitemap

        override def defaultRequestValue: Box[List[String]] = Full(thisPath)
      }
    }

    locs
  }

  def isLocCurrent(allLocs: List[Loc[_]], loc: Loc[_]) = allLocs.lastOption.map(_ == loc).getOrElse(false)
}

trait C3ResourceHelpers {
  import net.liftweb.util.TimeHelpers._

  val group: Group

  def toCss(directory: C3Directory) = {
    val owner: Box[User] = directory.metadata.get("x-c3-web-owner") match {
      case Some(id) => User.find(By(User.id, id.toLong))
      case _ => Empty
    }
    ".owner *" #> owner.map(_.shortName).getOrElse("Unknown") &
    ".owner [href]" #> owner.map(_.createLink) &
    ".name *" #> directory.name &
    ".link [href]" #> (directory.name + "/") &
    ".icon [src]" #> "/images/folder_classic.png" &
    ".created_date *" #> internetDateFormatter.format(directory.date)
  }

  def toCss(file: C3File) = {
    val owner: Box[User] = file.metadata.get("x-c3-web-owner") match {
      case Some(id) => User.find(By(User.id,id.toLong))
      case _ => Empty
    }
    ".owner *" #> owner.map(_.shortName).getOrElse("Unknown") &
    ".owner [href]" #> owner.map(_.createLink) &
    ".name *" #> file.name &
    ".link [href]" #> file.name &
    ".icon [src]" #> "/images/document_letter.png" &
    ".created_date *" #> internetDateFormatter.format(file.date)
  }

  def fileDownloadUrl(file: C3File): String = "/download" + file.fullname + "?dl=true"
  def fileViewUrl(file: C3File): String = "/download" + file.fullname
}

trait FSHelpers {

  case class FSLoc(path: List[String]){
    def title: String = path.lastOption.getOrElse("N/A")
  }

  def transformToPathLists(fullPath: List[String]): List[List[String]] = {
    @tailrec
    def transform(acc: List[List[String]], fullPath: List[String]): List[List[String]] = {
      fullPath match {
        case x :: Nil => acc
        case _ :: tail => transform(tail.reverse :: acc, tail)
        case Nil => acc
      }
    }

    if (fullPath.isEmpty) Nil
    else transform(List(fullPath), fullPath.reverse)
  }


}
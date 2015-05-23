package org.aphreet.c3.snippet.groups.snippet

import com.ifunsoftware.c3.access.C3System._
import com.ifunsoftware.c3.access.fs.{ C3Directory, C3File, C3FileSystemNode }
import com.ifunsoftware.c3.access.{ C3System, MetadataRemove, MetadataUpdate, StringMetadataValue }
import net.liftweb.common.{ Full, _ }
import net.liftweb.http.js.JE.{ JsRaw, JsVar }
import net.liftweb.http.js.JsCmds.{ Function, Script }
import net.liftweb.http.js.jquery.JqJsCmds
import net.liftweb.http.js.{ JsCmd, JsCmds }
import net.liftweb.http.{ RequestVar, S, SHtml, SessionVar }
import net.liftweb.sitemap.Loc.{ Hidden, Link, LinkText }
import net.liftweb.sitemap.{ Loc, Menu, SiteMap }
import net.liftweb.util.Helpers._
import net.liftweb.util.{ CssSel, PassThru }
import org.aphreet.c3.acl.groups.{ GroupsAccess, UserStatusGroup }
import org.aphreet.c3.comet.{ JournalServer, JournalServerEvent, MessageServerFactory }
import org.aphreet.c3.lib.DependencyFactory
import org.aphreet.c3.lib.metadata.Metadata
import org.aphreet.c3.lib.metadata.Metadata._
import org.aphreet.c3.loc.SuffixLoc
import org.aphreet.c3.model.{ Group, User }
import org.aphreet.c3.service.groups.GroupService
import org.aphreet.c3.service.journal.EventType
import org.aphreet.c3.snippet.LiftMessages
import org.aphreet.c3.snippet.groups.{ AbstractGroupPageLoc, GroupPageFilesData }
import org.aphreet.c3.snippet.groups.snippet.tags.TagForms
import org.aphreet.c3.util.helpers._

import scala.xml.{ NodeSeq, Text }

object GroupPageUpload extends AbstractGroupPageLoc[GroupPageFilesData] with SuffixLoc[Group, GroupPageFilesData] {

  override val name = "Upload"
  override val pathPrefix = "groups" :: Nil
  override val pathSuffix = "upload" :: Nil

  override def getItem(id: String) = Group.findById(id)

  override def isAccessiblePage(page: GroupPageFilesData): Boolean = {
    if (!page.isDirectoryLoc) {
      true
    } else {
      super.isAccessiblePage(page)
    }
  }

  // we don't use it here
  override def wrapItem(groupBox: Box[Group]): Box[GroupPageFilesData] = Empty

  override def link = {
    new Link[GroupPageFilesData](pathPrefix ++ pathSuffix) {
      override def pathList(value: GroupPageFilesData): List[String] = pathPrefix ::: value.group.getId :: Nil ::: pathSuffix ::: value.path
    }
  }

  override def finishPath(itemBox: => Box[Group],
                          restPath: List[String],
                          suffix: String = ""): Box[GroupPageFilesData] = {

    val resultPath = if (suffix.isEmpty) restPath.diff(pathSuffix) else restPath.diff(pathSuffix).init ::: List(restPath.diff(pathSuffix).last + "." + suffix)
    if (restPath.startsWith(pathSuffix)) wrapItemWithPath(itemBox, resultPath) else Empty
  }

  def wrapItemWithPath(groupBox: Box[Group], path: List[String]) = groupBox.map(GroupPageFilesData(_, path))
}

class GroupPageUpload(data: GroupPageFilesData) extends C3ResourceHelpers
    with GroupPageHelper with FSHelper with TagForms with C3AccessHelpers {

  import org.aphreet.c3.lib.DependencyFactory._

  override lazy val activeLocId = "upload"
  override lazy val group = data.group
  lazy val path = data.path
  val pathLocs = buildPathLocs
  val groupFilesLink = group.createLink
  val file = group.getFile(data.currentAddress)
  val currentResource = file.openOrThrowException("Directory or file is not exist.")
  val aclFormId = "acl"
  val defaultValueCheckbox = false
  private val logger = Logger(classOf[GroupPageFiles])
  private val c3 = inject[C3System].openOrThrowException("C3 is not available")
  private val journalServer: Box[JournalServer] = Full(MessageServerFactory(group))
  var currentResourceName = ""
  var currentAclValue = ""

  def parentNodeLink: String = pathLocs.reverse match {
    case Nil => groupFilesLink
    case xs  => xs.tail.headOption.fold(groupFilesLink)((l: Loc[_]) => l.createDefaultLink.get.text)
  }

  def render = {
    def renameCurrentNode(newName: String): JsCmd = {
      file.foreach {
        f =>
          val newPath = (f.fullname.split("/").toList.init ::: newName :: Nil).mkString("", "/", "")
          f.move(newPath)
          journalServer.foreach(_ ! JournalServerEvent(User.currentUserUnsafe, group, EventType.UpdateResources, newPath))
      }

      val redirectPath: String = file.map {
        f =>
          groupFilesLink + "/" + (path.init ::: newName :: Nil).mkString("", "/", if (f.isDirectory) "/" else "")
      }.openOr("")

      JsCmds.RedirectTo(redirectPath)
    }

    ".base_files_path *" #> (
      ".link [href]" #> groupFilesLink &
      ".link *" #> group.name.is) &
      ".bcrumb *" #> pathLocs.map {
        (loc: Loc[_]) =>
          (if (isLocCurrent(pathLocs, loc) && (hasSuperAccessResource(currentResource) || hasWriteAccessResource(currentResource))) {
            ".link" #>
              <span class="hide name_submit_func">
                {
                  Script(
                    Function("renameNodeCallback", List("name"),
                      SHtml.ajaxCall(
                        JsVar("name"),
                        (name: String) => renameCurrentNode(name))._2.cmd))
                }
              </span>
              <a href="#" id="node_name" data-type="text" data-pk="2" data-placeholder="Name..." data-original-title="Rename" class="editable editable-click">
                { loc.title }
              </a>
          } else {
            ".link [href]" #> loc.createDefaultLink &
              ".link *" #> loc.title
          }) &
            ".divider" #> (
              data.isDirectoryLoc match {
                case false if loc == pathLocs.last => (_: NodeSeq) => NodeSeq.Empty // if it is a file then we want to skip last "/" divider
                case _                             => PassThru
              })
      } &
      ".current_path *" #> Text(data.currentAddress) &
      ".edit_access *" #> acl()
  }

  //current edit resource
  def currentResource(nameResource: String, acl: String) = {
    currentResourceName = nameResource
    currentAclValue = acl
    JsCmds.Noop
  }

  def toCss(directory: C3Directory) = {

    val owner = nodeOwner(directory)
    val metaACL = acl(directory.metadata.get(ACL_META).getOrElse(""))
    def redirectToDirectory: CssSel = {
      ".link [href]" #> (directory.name + "/") &
        ".child_td [onclick]" #> SHtml.ajaxInvoke(() => JsCmds.RedirectTo(directory.name + "/"))
    }

    def transferDirectory: CssSel = {
      ".acl_cont [ondrag]" #> SHtml.ajaxInvoke(() => FileTransferHelper.saveDraggableResourceName(directory.name)) &
        ".acl_cont [ondrop]" #> SHtml.ajaxInvoke(() => FileTransferHelper.moveSelectedResource(group, data.currentAddress, directory.name, false))
    }

    def accessRestricted: CssSel = {
      ".link [href]" #> "#" &
        ".child_td [onclick]" #> SHtml.ajaxInvoke(() => LiftMessages.ajaxError(S.?("access.restricted")))
    }
    (if (hasSuperAccessResource(directory)) {
      ".rules *" #> metaACL &
        ".rules [id]" #> directory.fullname.hashCode &
        ".rules [onclick]" #> SHtml.ajaxInvoke(() => currentResource(directory.fullname.hashCode.toString, metaACL)) &
        transferDirectory &
        ".child_td [onclick]" #> SHtml.ajaxInvoke(() => JsCmds.RedirectTo(directory.name + "/")) &
        ".link [href]" #> (directory.name + "/")
    } else {
      val haveReadRight = checkReadAccessResource(directory)
      val haveWriteRight = hasWriteAccessResource(directory)
      (if (haveReadRight) {

        val groupAccess = new GroupsAccess {}
        User.currentUser match {
          case Full(u) => groupAccess.checkAccess(u, group) match {
            case UserStatusGroup.Admin | UserStatusGroup.Owner | UserStatusGroup.Member | UserStatusGroup.Other =>
              redirectToDirectory
            case UserStatusGroup.Request =>
              if (haveReadRight)
                redirectToDirectory
              else
                accessRestricted
          }
          case Empty =>
            redirectToDirectory
        }
      } else {
        accessRestricted
      }) &
        (if (haveWriteRight) {
          transferDirectory
        } else {
          ".acl_cont [ondrag]" #> ""
          ".acl_cont [ondrop]" #> ""
        }) &
        ".acl_cont *" #> metaACL
    }) &
      ".owner *" #> owner.map(_.shortName).getOrElse("Unknown") &
      ".owner [href]" #> owner.map(_.createLink) &
      ".name *" #> directory.name &
      ".icon [src]" #> "/images/folder_classic.png" &
      ".description_box *" #> directory.metadata.get(DESCRIPTION_META).getOrElse("") &
      ".created_date *" #> internetDateFormatter.format(directory.date)

  }

  def toCss(file: C3File) = {

    def transferFile: CssSel = {
      ".acl_cont [ondrag]" #> SHtml.ajaxInvoke(() => FileTransferHelper.saveDraggableResourceName(file.name)) &
        ".acl_cont [ondrop]" #> ""
    }

    val owner = nodeOwner(file)
    val metaACL = acl(file.metadata.get(ACL_META).getOrElse(""))

    (if (hasSuperAccessResource(file)) {
      ".rules *" #> metaACL &
        ".rules [id]" #> file.fullname.hashCode &
        ".rules [onclick]" #> SHtml.ajaxInvoke(() => currentResource(file.fullname.hashCode.toString, metaACL)) &
        transferFile &
        ".link [href]" #> file.name &
        ".child_td [onclick]" #> SHtml.ajaxInvoke(() => JsCmds.RedirectTo(file.name))
    } else {
      (if (checkReadAccessResource(file)) {
        ".link [href]" #> (file.name + "/") &
          ".child_td [onclick]" #> SHtml.ajaxInvoke(() => JsCmds.RedirectTo(file.name))
      } else {
        ".link [href]" #> "#" &
          ".child_td [onclick]" #> SHtml.ajaxInvoke(() => LiftMessages.ajaxError(S.?("access.restricted")))
      }) &
        (if (hasWriteAccessResource(file)) {
          transferFile
        } else {
          ".acl_cont [ondrag]" #> ""
          ".acl_cont [ondrop]" #> ""
        }) &
        ".acl_cont *" #> metaACL
    }) &
      ".owner *" #> owner.map(_.shortName).getOrElse("Unknown") &
      ".owner [href]" #> owner.map(_.createLink) &
      ".name *" #> ConvertHelper.ShortString(file.name, 40) &
      ".description_box *" #> ConvertHelper.ShortString(file.metadata.get(DESCRIPTION_META).getOrElse(""), if (file.name.length > 40) 60 else (110 - file.name.length)) &
      ".icon [src]" #> (file.metadata.get(CONTENT_TYPE) match {
        case Some("application/vnd.ms-excel")     => "/images/excel_type.png"
        case Some("image/png")                    => "/images/png_type.png"
        case Some("image/gif")                    => "/images/gif_type.png"
        case Some("application/pdf")              => "/images/pdf_type.png"
        case Some("application/msword")           => "/images/word_type.png"
        case Some("application/zip")              => "/images/zip_type.png"
        case Some("application/x-rar-compressed") => "/images/rar_type.png"
        // case Some(s) => if s.contains("text/plain") "/images/document_letter.png"
        case _                                    => "/images/unkhown_type.png"
      }) &
      ".created_date *" #> internetDateFormatter.format(file.date)
    //40 - max vizible symbols, when size file name is big
    //110 - count visible symbols in description columns
  }

  def acl(): CssSel = {
    def groupReadSave(b: Boolean): JsCmd = {
      group.getChildren(data.currentAddress).map(res => {
        if (res.fullname.hashCode.toString == currentResourceName) {
          val aclVal = if (b) "r---" else "----"
          currentAclValue = aclVal
        }
      })
      JsCmds.Noop
    }

    def groupWriteSave(b: Boolean): JsCmd = {
      group.getChildren(data.currentAddress).map(res => {
        if (res.fullname.hashCode.toString == currentResourceName) {
          val metaACL = acl(res.metadata.get(ACL_META).getOrElse(""))
          if (b) {
            val newACL = metaACL.toCharArray
            newACL.update(1, 'w')
            newACL.update(0, 'r')
            var aclVal = ""
            newACL.map(aclVal += _)
            currentAclValue = aclVal
          } else {
            val newAcl = metaACL.replace('w', '-')
            currentAclValue = newAcl
          }
        }
      })
      JsCmds.Noop
    }

    def otherUsersReadSave(b: Boolean): JsCmd = {
      group.getChildren(data.currentAddress).map(res => {
        if (res.fullname.hashCode.toString == currentResourceName) {
          val metaACL = acl(res.metadata.get(ACL_META).getOrElse(""))
          if (b) {
            val newACL = metaACL.toCharArray
            newACL.update(2, 'r')
            newACL.update(0, 'r')
            var aclVal = ""
            newACL.map(aclVal += _)
            currentAclValue = aclVal
          } else {
            val newACL = metaACL.toCharArray
            newACL.update(3, '-')
            newACL.update(2, '-')
            var aclVal = ""
            newACL.map(aclVal += _)
            currentAclValue = aclVal
          }
        }
      })
      JsCmds.Noop
    }

    def otherUsersWriteSave(b: Boolean): JsCmd = {
      group.getChildren(data.currentAddress).map(res => {
        if (res.fullname.hashCode.toString == currentResourceName) {
          val metaACL = acl(res.metadata.get(ACL_META).getOrElse(""))
          if (b)
            currentAclValue = "rwrw"
          else {
            val newACL = metaACL.toCharArray
            newACL.update(3, '-')
            var aclVal = ""
            newACL.map(aclVal += _)
            currentAclValue = aclVal
          }
        }
      })
      JsCmds.Noop
    }

    ".group_read" #> SHtml.ajaxCheckbox(defaultValueCheckbox, groupReadSave) &
      ".group_write" #> SHtml.ajaxCheckbox(defaultValueCheckbox, groupWriteSave) &
      ".all_read" #> SHtml.ajaxCheckbox(defaultValueCheckbox, otherUsersReadSave) &
      ".all_write" #> SHtml.ajaxCheckbox(defaultValueCheckbox, otherUsersWriteSave)
  }

  def buildPathLocs: List[Loc[_]] = {
    val locs: List[Loc[_]] = transformToPathLists(data.path).map {
      thisPath =>

        new Loc[List[String]] {
          private val __sitemap = SiteMap.build(Array(Menu(this)))

          override def name: String = thisPath.lastOption.getOrElse("N/A")

          override def link: Loc.Link[List[String]] = new Link[List[String]](List("groups", data.group.getId, "files") ::: thisPath)

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

  //list keys for metadata
  object keys extends SessionVar[Set[String]](Set())
}

package org.aphreet.c3
package comet

import java.util.Date

import net.liftmodules.textile.TextileParser
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JE.JsVar
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds
import net.liftweb.http.js.jquery.JqJsCmds.PrependHtml
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.util.Helpers
import org.aphreet.c3.model.{Group, User}
import org.aphreet.c3.service.journal._
import org.aphreet.c3.util.C3Exception
import org.aphreet.c3.util.helpers.DateTimeHelper

import scala.language.postfixOps
import scala.xml.{NodeSeq, Text, Unparsed}

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
trait GroupMessagesLog extends CometActor with CometListener {

  // fixedRender is always appended to render(..)
  override lazy val fixedRender: Box[NodeSeq] = Empty

  private lazy val liChat = liId.
    flatMap {
    Helpers.findId(defaultHtml, _)
  } openOr NodeSeq.Empty

  private lazy val liInfo = ilInfoId.
    flatMap {
    Helpers.findId(defaultHtml, _)
  } openOr NodeSeq.Empty

  private val logger: Logger = Logger(classOf[GroupMessagesLog])
  private val group: Box[Group] = S.attr("group_id").flatMap(Group.find)
  private val journalServer: Box[JournalServer] = group.map(MessageServerFactory(_))
  /* need these vals to be set eagerly, within the scope
   * of Comet component constructor
   */
  private val ulId = S.attr("ul_id") openOr "some_ul_id"
  private val liId = S.attr("li_id")

  private val ulInfoId = S.attr("ul_info") openOr "some_ul_id"
  private val ilInfoId = S.attr("li_info")

  private val inputTextContainerId = S.attr("input_container_id") openOr "input_container"
  private val inputCommentContainerid = S.attr("comment_input_id") openOr "comment_input_container"

  private var entities: List[JournalEntity] = Nil

  private var infoEntities: List[JournalEntity] = Nil

  private var currentShowingMsg = ""

  // handle an update to the message logs
  // by diffing the lists and then sending a partial update
  // to the browser
  override def lowPriority = {
    case JournalServerUpdate(value) =>

      val diffUpdate  = (value filterNot (entities contains))

      val update = diffUpdate.take(20).reverseMap {
        case e: Event => PrependHtml(ulId, line(e, liChat))
        case m: Message => PrependHtml(ulId, line(m, liChat))
      }

      partialUpdate(update)
      entities = value

      if (currentShowingMsg.isEmpty) {
        currentShowingMsg = getEntitryId(entities.head)
      }

      val diffInfo: List[JournalEntity] = value.filterNot(infoEntities contains).filter {
        case e: Event => (e.uuid == currentShowingMsg)
        case m: Message => (m.uuid == currentShowingMsg || m.parent.getOrElse("none") == currentShowingMsg)
      }

      val infoUpdate = createUpdatePostInfo(diffInfo)

      partialUpdate(infoUpdate)
      infoEntities :::= diffInfo

    case _ => logger.error("Not sure how we got here.")
  }

  // render the whole list of messages
  override def render = {

    "name=user_name" #> User.currentUser.map(_.shortName) &
      ("#" + ulId + " *") #> displayList &
      (s"#$ulInfoId *") #> displayListInfo &
      ("#" + inputTextContainerId + " *") #> { (xml: NodeSeq) => {
        var content = ""

        def sendMessage(): JsCmd = {
          journalServer.foreach(_ ! JournalServerMsg(User.currentUser.open_!, group.open_!, content, tags))
          tags.set(Nil)

          SetValById("postit", "") &
            JsCmds.Run("$('#" + inputTextContainerId + "').modal('hide');")
        }

        SHtml.ajaxForm {
          ".edit_tags_form_func *" #> {
            Script(
              Function("updateTagsCallback", List("tags"),
                SHtml.ajaxCall(
                  JsVar("tags"),
                  (d: String) => updateTags(d, tags))._2.cmd))
          } &
            "#tags_input *" #> Text("") &
            "#postit" #> SHtml.onSubmit((s: String) => content = s.trim) &
            "type=submit" #> ((xml: NodeSeq) => xml ++ SHtml.hidden(sendMessage)) apply xml
        }
      }
      } &
      ("#" + inputCommentContainerid + " *") #> { (xml: NodeSeq) => {
        var content = ""

        def sendComment(): JsCmd = {
          journalServer.foreach(_ ! JournalServerComment(User.currentUser.open_!, group.open_!, content, commentTags, currentShowingMsg))
          commentTags.set(Nil)

          SetValById("comment_postit", "") &
            JsCmds.Run("$('#" + inputCommentContainerid + "').modal('hide');")
        }

        SHtml.ajaxForm {
          ".edit_tags_form_func *" #> {
            Script(
              Function("updateCommentTagsCallback", List("tags"),
                SHtml.ajaxCall(
                  JsVar("tags"),
                  (d: String) => updateTags(d, commentTags))._2.cmd))
          } &
            "#comment_tags_input *" #> Text("") &
            "#comment_postit" #> SHtml.onSubmit((s: String) => content = s.trim) &
            "type=submit" #> ((xml: NodeSeq) => xml ++ SHtml.hidden(sendComment)) apply xml
        }
      }
      }

  }

  // display a list of messages
  private def displayList: NodeSeq = entities.take(20).flatMap {
    case e: Event => line(e, liChat)
    case m: Message => line(m, liChat)
  }

  private def displayListInfo: NodeSeq = infoEntities.flatMap {
    case e: Event => line(e, liInfo)
    case m: Message => line(m, liInfo)
  }

  // display a line
  private def line(c: Message, template: NodeSeq) = {
    val parent = c.parent match {
      case Some(p) => new Tuple2(p, "icon-comment")
      case _ => new Tuple2(c.uuid, "icon-envelope")
    }

    ("name=when *" #> formatMsgCreationDate(c.creationDate) &
      "name=who *" #> c.author.map(_.shortName) &
      "name=body *" #> toHtml(c.content) &
      ".msg_id [id]" #> ("msg-" + c.uuid.toString) &
      "i [class]" #>  parent._2 &
      ".show-more [onclick]" #> SHtml.ajaxInvoke(() => showAllComments(parent._1)) &
      ".tags *" #> {
        ".tag *" #> c.tags.map { (tag: String) =>
          <span class="label label-info">
            {tag}
          </span>
        }
      })(template)
  }

  private def showAllComments(uuid: String): JsCmd = {

    currentShowingMsg = uuid

    infoEntities = entities.filter {
      case e: Event => (e.uuid == currentShowingMsg)
      case m: Message => (m.uuid == currentShowingMsg || m.parent.getOrElse("none") == currentShowingMsg)
    }

    val updatePostInfo = JqJsCmds.EmptyAfter(ulInfoId, NodeSeq.Empty) :: createUpdatePostInfo(infoEntities)

    partialUpdate(updatePostInfo)
  }

  /**
   * Convert an incoming string into XHTML using Textile Markup
   *
   * @param msg the incoming string
   *
   * @return textile markup for the incoming string
   */
  def toHtml(msg: String): NodeSeq = TextileParser.paraFixer(TextileParser.toHtml(msg, Empty))

  private def formatMsgCreationDate(date: Date): String = DateTimeHelper.todayTimeOrPastDate(date)

  // display a line
  private def line(e: Event, template: NodeSeq) = {
    val resourceName = e.path.split("/").last
    val fullPath = "/groups" + e.path
    val tuple = e.eventType match {
      case EventType.ApproveUserToGroup =>
        val user = User.findByEmail(e.path).openOrThrowException("User not found")
        val msgBody = "Approved user <a href=\"" + user.createLink.toString() + "\">" + user.shortName + "</a>"
        val icon = "icon-check"
        (msgBody, icon)
      case EventType.CreateResources =>
        val msgBody = "Created resource <a href=\"" + fullPath + "\">" + resourceName + "</a>"
        val icon = "icon-download-alt"
        (msgBody, icon)
      case EventType.UpdateResources =>
        val msgBody = "Updated resource <a href=\"" + fullPath + "\">" + resourceName + "</a>"
        val icon = "icon-refresh"
        (msgBody, icon)
      case EventType.MoveResources =>
        val msgBody = "Moved resource <a href=\"" + fullPath + "\">" + resourceName + "</a> to " + e.path
        val icon = "icon-random"
        (msgBody, icon)
    }

    ("name=when *" #> formatMsgCreationDate(e.creationDate) &
      "name=who *" #> e.author.map(_.shortName) &
      "name=body *" #> Unparsed(tuple._1) &
      "i [class]" #> Unparsed(tuple._2) &
      ".show-more [onclick]" #> SHtml.ajaxInvoke(() => showAllComments(e.uuid)) &
      ".msg_id [id]" #> ("msg-" + e.uuid.toString) //      ".tags *" #> {
      //        ".tag *" #> c.tags.map { (tag: String) =>
      //          <span class="label label-info">{ tag }</span>
      //        }
      //      }
      )(template)
  }

  protected def updateTags(tagsInput: String, tag: SessionVar[List[String]]): JsCmd = {
    val tagList = if (tagsInput.isEmpty) Nil else tagsInput.split(',').map(_.trim).toList
    tag.set(tagList)
    JsCmds.Noop // bootstrap-editable will update text value on page by itself
  }

  // setup the component
  override def localSetup() {
    super.localSetup()
  }

  // register as a listener
  override def registerWith = {
    if (journalServer.isEmpty)
      throw new C3Exception("Cannot instantiate group message log outside group context!")
    else journalServer.open_!
  }

  object tags extends SessionVar[List[String]](Nil)

  object commentTags extends SessionVar[List[String]](Nil)

  private def getEntitryId(entity: JournalEntity) = entity match {
    case e: Event => e.uuid
    case m: Message => m.parent match {
      case Some(c) => c
      case _ => m.uuid
    }
  }

  private def createUpdatePostInfo(entities: List[JournalEntity]) = {
    entities.reverseMap {
      case e: Event => PrependHtml(ulInfoId, line(e, liInfo))
      case m: Message => PrependHtml(ulInfoId, line(m, liInfo))
    }
  }
}


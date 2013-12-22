package org.aphreet.c3
package comet

import org.aphreet.c3.util.C3Exception
import org.aphreet.c3.model.{Group, User, Message}
import org.aphreet.c3.util.helpers.DateTimeHelpers

import net.liftweb.util.Helpers
import net.liftmodules.textile.TextileParser
import net.liftweb.common._
import net.liftweb.http._
import js.{JsCmds, JsCmd}
import js.jquery.JqJsCmds.PrependHtml
import js.JE.JsVar
import js.JsCmds._

import scala.xml.{Text, NodeSeq}
import scala.language.postfixOps
import java.util.Date

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
trait GroupMessagesLog extends CometActor with CometListener {

  private val logger: Logger = Logger(classOf[GroupMessagesLog])

  private val group: Box[Group] = S.attr("group_id").flatMap(Group.find(_))
  private val messageServer: Box[MessageServer] = group.map(MessageServerFactory(_))

  private var messages: List[Message] = Nil


  /* need these vals to be set eagerly, within the scope
   * of Comet component constructor
   */
  private val ulId = S.attr("ul_id") openOr "some_ul_id"
  private val liId = S.attr("li_id")

  private val inputTextContainerId = S.attr("input_container_id") openOr "input_container"

  private lazy val li = liId.
    flatMap{ Helpers.findId(defaultHtml, _) } openOr NodeSeq.Empty

  // handle an update to the message logs
  // by diffing the lists and then sending a partial update
  // to the browser
  override def lowPriority = {
    case MessageServerUpdate(value) => {
      val update = (value filterNot (messages contains)).reverse.
        map(b => PrependHtml(ulId, line(b)))

      partialUpdate(update)
      messages = value
    }
    case _ => logger.error("Not sure how we got here.")
  }

  // fixedRender is always appended to render(..)
  override lazy val fixedRender: Box[NodeSeq] = Empty

  // display a line
  private def line(c: Message) = {
    ("name=when *" #> formatMsgCreationDate(c.creationDate) &
     "name=who *" #> c.author.map(_.shortName) &
     "name=body *" #> toHtml(c.content) &
     ".msg_id [id]"#> ("msg-" + c.uuid.toString) &
      ".tags *" #> {
       ".tag *" #> c.tags.map{ (tag: String) =>
         <span class="label label-info">{tag}</span>
       }
     })(li)
  }

  // display a list of messages
  private def displayList: NodeSeq = messages.flatMap(line)

  object tags extends SessionVar[List[String]](Nil)

  protected def updateTags(tagsInput: String): JsCmd = {
    val tagList = if (tagsInput.isEmpty) Nil else tagsInput.split(',').map(_.trim).toList
    tags.set(tagList)
    JsCmds.Noop // bootstrap-editable will update text value on page by itself
  }

  // render the whole list of messages
  override def render = {

    "name=user_name" #> User.currentUser.map(_.shortName) &
    ("#" + ulId + " *") #> displayList &
    ("#" + inputTextContainerId + " *") #> { (xml: NodeSeq) => {
      var content = ""

      def sendMessage(): JsCmd = {
        messageServer.foreach(_ ! MessageServerMsg(User.currentUser.open_!, group.open_!, content, tags))
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
                (d: String) => updateTags(d)
              )._2.cmd
            )
          )
        } &
        "#tags_input *" #> Text("") &
        "#postit" #> SHtml.onSubmit((s: String) => content = s.trim) &
        "type=submit" #> ((xml: NodeSeq) => xml ++ SHtml.hidden(sendMessage _)) apply(xml)
      }
    }}
  }

  // setup the component
  override def localSetup() {
    super.localSetup()
  }

  // register as a listener
  override def registerWith = {
    if(messageServer.isEmpty)
      throw new C3Exception("Cannot instantiate group message log outside group context!")
    else messageServer.open_!
  }

  /**
   * Convert an incoming string into XHTML using Textile Markup
   *
   * @param msg the incoming string
   *
   * @return textile markup for the incoming string
   */
  def toHtml(msg: String): NodeSeq = TextileParser.paraFixer(TextileParser.toHtml(msg, Empty))

  private def formatMsgCreationDate(date: util.Date): String = DateTimeHelpers.todayTimeOrPastDate(date)

}


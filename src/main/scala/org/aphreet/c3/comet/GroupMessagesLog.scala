package org.aphreet.c3.comet

import net.liftweb.http._
import net.liftweb.common.{Empty, Box, Full}
import org.aphreet.c3.common.C3Exception
import org.aphreet.c3.model.{Group, User, Message}
import net.liftweb.util.Helpers
import xml.NodeSeq
import js.jquery.JqJsCmds.{PrependHtml, AppendHtml}
import net.liftweb.util.TimeHelpers._
import net.liftweb.http.js.JsCmds._
import org.aphreet.c3.lib.group.GroupContextAware
import net.liftweb.common.Full

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
class GroupMessagesLog extends CometActor with CometListener with GroupContextAware {

  private var messages: List[Message] = Nil
  private var messageServer: Box[MessageServer] = Empty

  /* need these vals to be set eagerly, within the scope
   * of Comet component constructor
   */
  private val ulId = S.attr("ul_id") openOr "some_ul_id"
  private val liId = S.attr("li_id")

  private lazy val li = liId.
    flatMap{ Helpers.findId(defaultHtml, _) } openOr NodeSeq.Empty

  private val inputId = Helpers.nextFuncName

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
  }

  // render the input area by binding the
  // appropriate dynamically generated code to the
  // view supplied by the template
  override lazy val fixedRender: Box[NodeSeq] = {
    S.runTemplate("_message_log_post_form" :: Nil).map( html =>
      SHtml.ajaxForm {
        ("#postit" #> SHtml.onSubmit((s: String) => {
          messageServer.foreach( _ ! MessageServerMsg(User.currentUser.open_!, currentGroup.open_! ,s.trim) )
          SetValById("postit", "")
        }) ).apply(html)
      }
    )
  }

  // display a line
  private def line(c: Message) = {
    ("name=when" #> toInternetDate(c.creationDate) &
     "name=who" #> c.author.map(_.shortName) &
     "name=body" #> c.content)(li)
  }

  // display a list of messages
  private def displayList: NodeSeq = messages.flatMap(line)

  // render the whole list of messages
  override def render = {
    "name=user_name" #> User.currentUser.map(_.shortName) &
    ("#"+ulId+" *") #> displayList
  }

  // setup the component
  override def localSetup() {
    super.localSetup()
  }

  // register as a listener
  override def registerWith = {
    currentGroup match {
      case Full(g: Group) => {
        val ms: MessageServer = MessageServerFactory(g)
        messageServer = Full(ms)
        ms
      }
      case _ => {
        throw new C3Exception("Cannot instantiate group message log outside group context!")
        null
      }
    }
  }

}


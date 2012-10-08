package org.aphreet.c3.comet

import net.liftweb.http._
import net.liftweb.common._
import org.aphreet.c3.util.C3Exception
import org.aphreet.c3.model.{Group, User, Message}
import net.liftweb.util.Helpers
import xml.NodeSeq
import js.jquery.JqJsCmds.PrependHtml
import net.liftweb.http.js.JsCmds._
import java.util
import java.text.SimpleDateFormat
import net.liftweb.common.Full
import net.liftweb.textile.TextileParser

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
class GroupMessagesLog extends CometActor with CometListener {

  private val logger = Logger(classOf[GroupMessagesLog])
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
    ("name=when" #> formatMsgCreationDate(c.creationDate) &
     "name=who" #> c.author.map(_.shortName) &
     "name=body" #> toHtml(c.content))(li)
  }

  // display a list of messages
  private def displayList: NodeSeq = messages.flatMap(line)

  // render the whole list of messages
  override def render = {

    "name=user_name" #> User.currentUser.map(_.shortName) &
    ("#"+ulId+" *") #> displayList &
    ("#" + inputTextContainerId + " *") #> { (xml: NodeSeq) => {
      SHtml.ajaxForm(("#postit" #> SHtml.onSubmit((s: String) => {
        messageServer.foreach( _ ! MessageServerMsg(User.currentUser.open_!, group.open_! ,s.trim) )
        SetValById("postit", "")
      })).apply(xml))
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

  private val customFormatter = new SimpleDateFormat("dd.MM.yyyy HH:mm")
  private def formatMsgCreationDate(date: util.Date): String = customFormatter.format(date)

}


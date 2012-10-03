package org.aphreet.c3.comet

import net.liftweb.http._
import net.liftweb.common._
import org.aphreet.c3.common.C3Exception
import org.aphreet.c3.model.{Group, User, Message}
import net.liftweb.util.Helpers
import xml.NodeSeq
import js.jquery.JqJsCmds.PrependHtml
import net.liftweb.http.js.JsCmds._
import org.aphreet.c3.lib.group.GroupContextAware
import java.util
import java.text.SimpleDateFormat
import net.liftweb.common.Full

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
case class UpdateGroupMapping(groupname: String)

class GroupMessagesLog extends CometActor with CometListener with GroupContextAware {

  private val logger = Logger(classOf[GroupMessagesLog])

  private var group: Box[Group] = Empty
  private var messages: List[Message] = Nil
  private var messageServer: Box[MessageServer] = Empty

  /* need these vals to be set eagerly, within the scope
   * of Comet component constructor
   */
  private val ulId = S.attr("ul_id") openOr "some_ul_id"
  private val liId = S.attr("li_id")

  private val inputTextContainerId = S.attr("input_container_id") openOr "input_container"

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
    case _ => logger.error("Not sure how we got here.")
  }

  override def highPriority = {
    case UpdateGroupMapping(groupname) => {
      group = Group.findByName(groupname)
      if (group.isEmpty)
        logger.error("Group " + groupname + " is not found!")
      else {
        logger.info("Updating GroupMessagesLog for group: %s".format(group.open_!.name.is))
        messageServer.foreach(_ ! RemoveAListener(this))
        logger.info("Registering comet actor: %s".format(this))
        MessageServerFactory.apply(group.open_!) ! AddAListener(this, shouldUpdate)
        reRender(sendAll = true) // update all
      }
    }
  }

  // fixedRender is always appended to render(..)
  override lazy val fixedRender: Box[NodeSeq] = Empty

  // display a line
  private def line(c: Message) = {
    ("name=when" #> formatMsgCreationDate(c.creationDate) &
     "name=who" #> c.author.map(_.shortName) &
     "name=body" #> c.content)(li)
  }

  // display a list of messages
  private def displayList: NodeSeq = messages.flatMap(line)

  // render the whole list of messages
  override def render = {

    "name=user_name" #> User.currentUser.map(_.shortName) &
    ("#"+ulId+" *") #> displayList &
    ("#" + inputTextContainerId + " *") #> { (xml: NodeSeq) => {
      SHtml.ajaxForm(("#postit" #> SHtml.onSubmit((s: String) => {
        messageServer.foreach( _ ! MessageServerMsg(User.currentUser.open_!, currentGroup.open_! ,s.trim) )
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
    currentGroup match {
      case Full(g: Group) => {
        group = Full(g)
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

  private val customFormatter = new SimpleDateFormat("dd.MM.yyyy HH:mm")
  private def formatMsgCreationDate(date: util.Date): String = customFormatter.format(date)

}


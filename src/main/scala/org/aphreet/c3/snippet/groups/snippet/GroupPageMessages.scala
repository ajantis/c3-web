package org.aphreet.c3.snippet.groups.snippet

import org.aphreet.c3.model.Group
import net.liftweb.common.{Logger, Box}
import xml.NodeSeq
import org.aphreet.c3.loc.SuffixLoc
import org.aphreet.c3.snippet.groups.{AbstractGroupPageLoc, GroupPageData}
import net.liftweb.sitemap.Loc.Link
import net.liftweb.util.Helpers._
/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */

object GroupPageMessages extends AbstractGroupPageLoc[GroupPageData] with SuffixLoc {
  override val name = "Messages"
  override val pathPrefix = "groups" :: Nil
  override val pathSuffix = "messages" ::  Nil
  override def getItem(id: String) = Group.find(id)
  override def wrapItem(groupBox: Box[Group]) = groupBox.map(new GroupPageData(_))

  override def link = {
    new Link[GroupPageData](pathPrefix ++ pathSuffix){
      override def pathList(value: GroupPageData): List[String] = pathPrefix ::: value.group.id.is.toString :: Nil ::: pathSuffix
    }
  }
}

class GroupPageMessages(data: GroupPageData) extends GroupPageHelpers{
  import GroupPageMessages._

  override lazy val group = data.group
  override lazy val activeLocId = "messages"

  private val logger = Logger(classOf[GroupPageMessages])

  def linkMessages = {
    val links = link.pathList(data).drop(1).mkString("/")
    ".message_button [href]" #> links
  }

  def putCometJournal(xml: NodeSeq): NodeSeq = putCometMessender("GroupJournal")(xml)

  def putCometChat(xml: NodeSeq): NodeSeq = putCometMessender("GroupChat")(xml)

  private def putCometMessender(cometActorName: String)(xml: NodeSeq): NodeSeq = {
    val actorName = "group-" + group.id.is + "-messages-log-comet-actor"

    <div class={"lift:comet?type=" + cometActorName + ";name=" + actorName + ";group_id="+ group.id.is +";ul_id=main_ul_id;li_id=main_li_id;input_container_id=input_container"}>
      {xml}
    </div>
  }

}
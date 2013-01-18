package org.aphreet.c3.snippet.group.snippet

import org.aphreet.c3.model.Group
import net.liftweb.common.{Logger, Box}
import xml.NodeSeq
import org.aphreet.c3.loc.SuffixLoc
import org.aphreet.c3.snippet.group.{AbstractGroupPageLoc, GroupPageData}
import net.liftweb.sitemap.Loc.Link

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
  override lazy val group = data.group
  override lazy val activeLocId = "messages"

  private val logger = Logger(classOf[GroupPageMessages])

  def putCometMessenger(xml: NodeSeq): NodeSeq = {
    val actorName = "group-" + group.id.is + "-messages-log-comet-actor"
    logger.debug("Using CometActor with name: %s".format(actorName))

    <div class={"lift:comet?type=GroupMessagesLog;name=" + actorName + ";group_id="+ group.id.is +";ul_id=main_ul_id;li_id=main_li_id;input_container_id=input_container"}>
      {xml}
    </div>
  }
}
package org.aphreet.c3.loc

import org.aphreet.c3.model.Group
import net.liftweb.common.Box
import org.aphreet.c3.apiaccess.C3

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */


sealed class GroupPage(groupName: String){
  lazy val group: Box[Group] = Group findByName groupName
}

case class GroupFilesPage(filePath: String, groupName: String) extends GroupPage(groupName)

case class GroupWikiPage(wikiName: String, groupName: String, edit: Boolean) extends GroupPage(groupName)

package org.aphreet.c3.loc

import net.liftweb.sitemap.Loc
import xml.{NodeSeq, Text}
import net.liftweb.util.Helpers._
import net.liftweb.common.Full

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */


trait GroupLoc[T <: GroupPage] extends Loc[T]{

  protected val basePrefixUrl = "group"

  protected val filesSuffix = "files"

  protected val wikiSuffix = "wiki"

}
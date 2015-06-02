package org.aphreet.c3.util

import net.liftweb.http.rest.RestHelper
import net.liftmodules.textile.TextileParser
import net.liftweb.http.S

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
//todo need refactoring
object TextileRenderer extends RestHelper {
  serve {
    case "markitup" :: "textile" :: "render" :: _ Post _ =>
      <span>{ TextileParser.toHtml(S.param("data").openOr("")) }</span>
  }
}
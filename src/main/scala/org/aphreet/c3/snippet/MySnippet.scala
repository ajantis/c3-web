package org.aphreet.c3.snippet

import net.liftweb.util.BindHelpers._
import org.aphreet.c3.model._
import net.liftweb.common.{Logger, Full, Empty}
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.mapper.By
import net.liftweb.util.TimeHelpers
import xml.{Text, NodeSeq}
import net.liftweb.http.{S}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http._
import org.aphreet.c3.apiaccess.C3
/**
 * Created with IntelliJ IDEA.
 * User: Serjk
 * Date: 27.04.12
 * Time: 10:52
 * To change this template use File | Settings | File Templates.
 */

class MySnippet {
  def prnt(html:NodeSeq):NodeSeq = {
    if (!S.param("MyName").openOr("").isEmpty()){
      val str = "Hello " + S.param("MyName").openOr("");

      bind("b",html, "text" -> str);

    }
    else{
      val str = "string empty";
      bind("b",html, "text" -> str);
    }


  }






}

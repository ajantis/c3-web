/**
 * Copyright (c) 2011, Mikhail Malygin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions
 * are met:
 *

 * 1. Redistributions of source code must retain the above copyright 
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above 
 * copyright notice, this list of conditions and the following disclaimer 
 * in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the IFMO nor the names of its contributors 
 * may be used to endorse or promote products derived from this software 
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.aphreet.c3.snippet

import net.liftweb.common.{Logger, Full}
import net.liftweb.util.BindHelpers._
import org.aphreet.c3.model.Wiki
import net.liftweb.http.{SHtml, S}
import be.devijver.wikipedia.SmartLink.SmartLinkType
import org.aphreet.c3.lib.wiki.C3HtmlVisitor
import be.devijver.wikipedia.{SmartLinkResolver, Parser, SmartLink}
import java.io.StringWriter
import org.aphreet.c3.view.GroupNavigationUtil
import xml.{Node, XML, NodeSeq}

class WikiSnippet{

  val logger = Logger(classOf[GroupForm])

  def view(html: NodeSeq) : NodeSeq = {

    val pageName = S.param("pagename") match {
      case Full(value) => value
      case _ => "Main"
    }

    val groupName = S.param("groupname") match {
      case Full(value) => value
      case _ => ""
    }

    Wiki.getPage(groupName, pageName) match {
      case Some(page) => {
        
        val map = Wiki.getMetadata(groupName, pageName)

        println(map)

        bind("wiki", html,
          "groupname" -> groupName,
          "groupnav" -> GroupNavigationUtil.createNavigation(groupName),

          "name" -> page.name,
          "content" -> XML.loadString(formatContent(page.content, groupName)),
          "actions" -> <a href={"/group/" + groupName + "/wiki/" + pageName + "/edit"}>Edit</a>,
          "metadata" -> {(ns:NodeSeq) =>
            (1 to 10).flatMap(el => bind("md", ns, "key" --> el, "value" --> el)):NodeSeq}
        )
      }


      case None => {
        bind("wiki", html,
          "groupname" -> groupName,
          "groupnav" -> GroupNavigationUtil.createNavigation(groupName),

          "name" -> pageName,
          "content" -> "Page not found",
          "actions" -> <a href={"/group/" + groupName + "/wiki/" + pageName + "/edit"}>Create</a>,
          "metadata" -> {(ns:NodeSeq) =>
            (1 to 10).flatMap(i => bind("md", ns, "key" --> i, "value" --> i)):NodeSeq}
        )
      }
    }
  }

  def formatContent(content:String, group:String):String = {

    val writer = new StringWriter

    new Parser().withVisitor(content.replaceAll("([^\r])\n", "$1\r\n"), new C3HtmlVisitor(writer, new SmartLinkResolver(){

      override def  resolve(key:String):SmartLink = {
        resolveSmartLink(key, group)
      }

    }));

    writer.toString
  }

  def resolveSmartLink(key:String, group:String):SmartLink = {
    new SmartLink("/group/" + group + "/wiki/" + key, key, SmartLinkType.A_LINK);
  }


  def edit(html: NodeSeq) : NodeSeq = {

    val pageName = S.param("pagename") match {
      case Full(value) => value
      case _ => "Main"
    }

    val groupName = S.param("groupname") match {
      case Full(value) => value
      case _ => ""
    }

    bind("wiki", html,
      "groupname" -> groupName,
      "groupnav" -> GroupNavigationUtil.createNavigation(groupName),
      "name" -> pageName,
      "actions" -> <a href={"/group/" + groupName + "/wiki/" + pageName}>Cancel</a>,
      "metadata" -> {(ns:NodeSeq) =>
        (1 to 10).flatMap(i => bind("md", ns, "key" --> i, "value" --> i)):NodeSeq}
    )

  }

  def form(html: NodeSeq) : NodeSeq = {

    var pageContent = ""

    val pageName = S.param("pagename") match {
      case Full(value) => value
      case _ => "Main"
    }

    val groupName = S.param("groupname") match {
      case Full(value) => value
      case _ => ""
    }

    def processWikiEdit() = {
      Wiki.getPage(groupName, pageName) match {
        case Some(page) => {

          page.content = pageContent;

          Wiki.savePage(groupName, page)
          S.redirectTo("/group/" + groupName + "/wiki/" + pageName)
        }

        case None => {

          val page = new Wiki(pageName, pageContent)

          Wiki.createPage(groupName, page)
          S.redirectTo("/group/" + groupName + "/wiki/" + pageName)
        }
      }
    }

    val pageString = Wiki.getPage(groupName, pageName) match {
      case Some(page) => page.content
      case None => ""
    }

    bind("wiki-form", html,
      "content" -> SHtml.textarea(pageString, pageContent = _),
      "submit" -> SHtml.submit("Save", processWikiEdit)
    )

  }

}
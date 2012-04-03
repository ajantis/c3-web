package org.aphreet.c3.snippet

import org.aphreet.c3.model.{Group, User}
import net.liftweb.mapper.MaxRows
import net.liftweb.sitemap.Loc
import net.liftweb.http.{GetRequest, Req, SHtml, S}
import xml.{XML, Text, NodeSeq}
import net.liftweb.common.Full
import net.liftweb.widgets.autocomplete.AutoComplete
import org.apache.commons.httpclient.util.URIUtil

/**
 * Copyright (c) 2011, Dmitry Ivanov
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

import net.liftweb.util.BindHelpers._
 
 
class MainSnippet  {

  def currentUser(html: NodeSeq) : NodeSeq = {
    User.currentUser match {
      case Full(user) => {
        bind("user", html,
          "email" -> user.email.is
        )
      }
      case _ => NodeSeq.Empty
    }
  }

  def myGroupsList(html: NodeSeq) : NodeSeq = {

     if(User.loggedIn_?) {
       bind("myGroupsList", html,
        "groups" -> { (ns: NodeSeq) =>
          User.currentUser.open_!.groups.flatMap(group =>
            bind("group", ns,
                 "name" -> group.asInstanceOf[Group].name.is)):NodeSeq
        }

       )
     }
     else {
       Text("Please, log in the system or sign up.")
     }
  }


  def featuredGroups(html: NodeSeq) : NodeSeq = {

    val MAX_FEATURED_GROUPS_ON_PAGE = 5

    val featuredGroups : List[Group] = Group.findAll(MaxRows(MAX_FEATURED_GROUPS_ON_PAGE))

    if(!featuredGroups.isEmpty)
      bind("featuredGroups", html,
      "groups" -> {(ns: NodeSeq) => featuredGroups.flatMap( group => bind("group", ns,
        "name" -> <a href={"/group/"+group.name}>{group.name}</a>)):NodeSeq})
    else Text("There are no groups in db currently.")

  }

  def breadCrumbs(html: NodeSeq) : NodeSeq = {
    val breadcrumbs: List[Loc[_]] =
      for {
        currentLoc <- S.location.toList
        loc <- currentLoc.breadCrumbs

      } yield loc


    if(S.param("rewrite").isEmpty) {
      bind("breadCrumbsMenu", html,
          "breadCrumbs" -> {(ns: NodeSeq) => {breadcrumbs.flatMap( loc =>
              if(! loc.createDefaultLink.get.text.contains("index"))

                bind("breadCrumb", ns,
                  "link" -> <a href={loc.createDefaultLink.get}>{loc.title}</a> //SHtml.link(loc.linkText.toString,() => {}, loc.title)


                )

              else NodeSeq.Empty
          ): NodeSeq} }
      )
    }
    else {
      S.param("rewrite").open_! match
      {
        case "groupFiles" => {
          val groupname = S.param("groupname").open_!

          // List[(link,name)]
          val groupDirLst : List[String] = S.param("groupdirectory").open_!.split("/").toList
          val groupDirLinkLst : List[(String,String)] = groupDirLst.map(dir => ("/group/"+groupname+"/files/" + {groupDirLst.takeWhile(_ != dir).mkString("/") match {
            case "" => ""
            case str => str + "/"
          } } + dir , dir))

          val brdCrmbList : List[(String,String)] = Tuple2("/group/"+groupname,groupname) :: Tuple2("/group/"+groupname+"/files", "Files") :: groupDirLinkLst

          bind("breadCrumbsMenu", html,
              "breadCrumbs" -> {(ns: NodeSeq) => {brdCrmbList.filter(_._2 != "").flatMap(linkWithName =>
                    bind("breadCrumb", ns,
                      "link" -> <a href={linkWithName._1}>{linkWithName._2}</a>
                    )
              ): NodeSeq} }
          )
        }
        case "groupOverview" => {
           val groupname = S.param("groupname").open_!
           val brdCrmbList : List[(String,String)] = Tuple2("/group/"+groupname,groupname) :: Nil

           bind("breadCrumbsMenu", html,
              "breadCrumbs" -> {(ns: NodeSeq) => {brdCrmbList.filter(_._2 != "").flatMap(linkWithName =>
                    bind("breadCrumb", ns,
                      "link" -> <a href={linkWithName._1}>{linkWithName._2}</a>
                    )
              ): NodeSeq} }
           )
        }
        case "vmOverview" => {

          val vmName = S.param("vmName").open_!

          val brdCrmbList : List[(String,String)] = ("/vmservice/","VM Service") :: ("/vmservice/vm/"+URIUtil.encodeQuery(vmName,"UTF-8"),vmName) :: Nil

          bind("breadCrumbsMenu", html,
               "breadCrumbs" -> {(ns: NodeSeq) => {
                 brdCrmbList.flatMap(linkWithName =>
                    bind("breadCrumb", ns,
                      "link" -> <a href={linkWithName._1}>{linkWithName._2}</a>
                    )
              ): NodeSeq} }
          )
        }
        case _ => NodeSeq.Empty // TODO implement
      }

    }


  }



  private val data = List(
    "Timothy","Derek","Ross","Tyler",
    "Indrajit","Harry","Greg","Debby")

  def sample(xhtml: NodeSeq): NodeSeq =
    bind("f", xhtml,
      "find_name" -> AutoComplete("", (current,limit) =>
        data.filter(_.toLowerCase.startsWith(current.toLowerCase)),
        value => println("Submitted: " + value))
    )


}
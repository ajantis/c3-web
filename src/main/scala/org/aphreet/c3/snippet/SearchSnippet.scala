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
package org.aphreet.c3.snippet

import xml.{Text, NodeSeq}
import net.liftweb.util.BindHelpers._
import java.text.SimpleDateFormat
import net.liftweb.common.{Logger, Box, Empty, Full}
import org.apache.commons.httpclient.util.URIUtil
import net.liftweb.http._
import org.aphreet.c3.model.{C3Path, Tag, Category, User}
import com.ifunsoftware.c3.access.{C3System, C3AccessException}
import org.aphreet.c3.lib.DependencyFactory._

class SearchSnippet extends StatefulSnippet{
  
  val c3 = inject[C3System].open_!

  val logger = Logger(classOf[SearchSnippet])

  var dispatch : DispatchIt = if(stringToSearch.isEmpty) {

    case "search" => searchForm _
    case "miniSearch" => miniSearchForm _
  }
  else {
    case "search" => resultPage _
    case "miniSearch" => miniSearchForm _
  }


  var searchString = ""

  def resultPage (html: NodeSeq) = {

    if (! stringToSearch.isEmpty )
      searchString = stringToSearch.open_!

    val resultEntries = c3.search(searchString)

    val format: SimpleDateFormat = new SimpleDateFormat("dd/MM/yyyy")

    bind("search", html,
      "query" -> SHtml.text(searchString, processQuery _ ,"placeholder" -> "Search","size" -> "60" ),

      "resultSet" -> { (ns : NodeSeq) =>
        resultEntries.flatMap( entry => {

          try {
            val resource = c3.getResource(entry.address, List("c3.ext.fs.path"))
            val metadata = resource.metadata

            val name = metadata.getOrElse("c3.fs.nodename", "")
            val path = resource.systemMetadata.getOrElse("c3.ext.fs.path", "")
            val resourceType = metadata.getOrElse("c3.fs.nodetype", "")

            val c3Path = C3Path(path)

            if(name != "")
              bind("entry", ns,
                "address" ->  entry.address ,
                "name" -> URIUtil.decode(name, "UTF-8"),
                "created" -> format.format(resource.date),
                "resource_path" -> Text(""),
                "full_path" -> SHtml.link(c3Path.resourceUri, () => {}, Text(name)),
                "type" -> { resourceType match {
                  case "directory" => <img src="/images/icons/folder.gif"/>
                  case _ => <img src="/images/icons/document.gif" />
                }}
              )
            else NodeSeq.Empty
          }
          catch {
            case e: C3AccessException => {
              logger error e
              NodeSeq.Empty
            }
          }

        }):NodeSeq
      },
      "submit" -> SHtml.submit("Go", () => {}  ),
      "user_categories" -> {(ns: NodeSeq ) => User.currentUser.open_!.categories.flatMap(
        (category: Category) =>
          bind("category",ns,
            "name" -> category.name.is,
            "tags" -> { (nss: NodeSeq) => category.tags.flatMap(
              (tag:Tag) =>
                bind("tag",nss,
                  "name" -> tag.name.is
                )
            ):NodeSeq }
          )
      ):NodeSeq}
    )
  }

  def searchForm (html: NodeSeq) = {


    bind("search", html,
      "query" -> SHtml.text(searchString, processQuery _ , "placeholder" -> "Search","size" -> "60"),
      "resultSet" -> "",
      "submit" -> SHtml.submit("Go", () => {
        dispatch = {
          case "search" => resultPage _
          case "miniSearch" => miniSearchForm _
        }
      }),
      "user_categories" -> {(ns: NodeSeq ) => User.currentUser.open_!.categories.flatMap(
        (category: Category) =>
          bind("category",ns,
            "name" -> category.name.is,
            "tags" -> { (nss: NodeSeq) => category.tags.flatMap(
              (tag:Tag) =>
                bind("tag",nss,
                  "name" -> tag.name.is
                )
            ):NodeSeq}
          )
      ):NodeSeq}
    )
  }

  // we store a string entered in an input box of mini search form
  object stringToSearch extends SessionVar[Box[String]](Empty)

  def miniSearchForm (html : NodeSeq) = {
    var searchParam = ""
    bind("miniSearch", html,
      "search_string" -> SHtml.text("",searchParam = _ , "placeholder" -> "Search"),
      "submit" -> SHtml.submit("Go", () => S.redirectTo("/search",() => {

        stringToSearch.set(Full(searchParam))
        User.addSearchRequest( searchParam )
      }))
    )
  }

  def processQuery(query : String){
    searchString = query
    stringToSearch(Full(query))

    if(searchString != ""){

      User.addSearchRequest( searchString )

      dispatch = {
        case "search" => resultPage _
        case "miniSearch" => miniSearchForm _

      }
    }
    else {
      S.error("Please, enter a text to search for")
      dispatch = {
        case "search" => searchForm _
        case "miniSearch" => miniSearchForm _
      }
    }
  }

}
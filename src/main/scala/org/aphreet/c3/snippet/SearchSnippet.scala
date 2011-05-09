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
import org.aphreet.c3.helpers.MetadataParser
import org.aphreet.c3.model.{Tag, Category, User}
import net.liftweb.util.BindHelpers._
import java.util.Date
import java.text.{SimpleDateFormat}
import net.liftweb.util.TimeHelpers
import net.liftweb.common.{Logger, Box, Empty, Full}
import org.apache.commons.lang.time.DateUtils
import org.apache.commons.httpclient.util.URIUtil
import net.liftweb.widgets.autocomplete.AutoComplete
import net.liftweb.http._
import org.aphreet.c3.apiaccess.{C3ClientException, C3Client}

class SearchSnippet extends StatefulSnippet {

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
  var resultSet = NodeSeq.Empty

  def resultPage (html: NodeSeq) = {

     if (! stringToSearch.isEmpty ) searchString = stringToSearch.open_!
     if(searchString!="") resultSet = C3Client().doSearch(searchString)

     bind("search", html,
      "query" -> SHtml.text(searchString, processQuery _ ,"placeholder" -> "Search","size" -> "60" ),
      // doesn't work
      /*"query" -> AutoComplete(searchString, (current: String,limit: Int) =>
        User.currentSearchRequests.filter(_.toLowerCase.startsWith(current.toLowerCase)),
        value => processQuery(value),
        attrs = ("placeholder","Search")
      ),*/
      "resultSet" -> { (ns : NodeSeq) =>
        (resultSet \\ "entry").flatMap( entry => {

          try {
            val metadata = C3Client().getResourceMetadataWithFSPath( (entry \ "@address").text )

            val mdParser = MetadataParser(metadata)

            val name: String = mdParser.getNodeWithAttributeValue("element","key","c3.fs.nodename") match {
              case NodeSeq.Empty => ""
              case xs => (xs \\ "value")(0) text
            }

            val path: String = mdParser.getNodeWithAttributeValue("element","key","c3.ext.fs.path") match {
              case NodeSeq.Empty => ""
              case xs => (xs \\ "value")(0) text
            }

            //(metadata \\ "element").toList.filter((element:NodeSeq) => (element \ "@key").toString == "c3.fs.nodetype")
            val resourceType: String = mdParser.getNodeWithAttributeValue("element","key","c3.fs.nodetype") match {
              case NodeSeq.Empty => ""
              case xs => (xs \\ "value")(0) text
            }


            //NOTE: ZZ on end is not compatible with jdk, but allows for formatting
            //dates like so (note the : 3rd from last spot, which is iso8601 standard):
            //date=2008-10-03T10:29:40.046-04:00
            val DATE_FORMAT_8601 = "yyyy-MM-dd'T'HH:mm:ss"
            logger error ((metadata \\ "resource")(0) \\ "@createDate")
            val format: SimpleDateFormat = new SimpleDateFormat("dd/MM/yyyy")

            val created: Date = ((metadata \\ "resource")(0) \\ "@createDate").text match {
              case "" => TimeHelpers.now
              case str => DateUtils.parseDate(str.split('.').head, Array(DATE_FORMAT_8601))
            }

            def parseToFolderPath(path: String): NodeSeq = {
              path.split("/").toList.tail match {
                case Nil => NodeSeq.Empty
                case lst => {
                  SHtml.link("/group/" + lst.reverse.tail.reverse.mkString("/"), () => {}, Text("Folder"))
                }
              }
            }

            if(name != "")
                bind("entry", ns,
                  "address" ->   { (entry \ "@address").text } ,
                  "name" -> URIUtil.decode(name, "UTF-8"),
                  "created" -> format.format(created),
                  "resource_path" -> { path.split("/").toList.tail match {
                    case Nil => NodeSeq.Empty
                    case lst => SHtml.link("/group/" + lst.mkString("/"), () => {}, Text(name))
                  }},
                  "full_path" -> { path.split("/").toList.tail match {
                    case Nil => NodeSeq.Empty
                    case fullPath @ (group :: "files" :: filePath) => {
                      (SHtml.link("/group/" + group + "/files" , () => {}, Text(group)):NodeSeq) ++
                      (
                        filePath.reverse match {
                          case file :: path =>
                            path.reverse.flatMap( i =>
                                (( Text("/") ++ SHtml.link("/group/"+group+"/files/"+filePath.takeWhile(_ != i).mkString("/") + i , () => {}, Text(i)) ):NodeSeq)
                            ): NodeSeq
                          case _ => NodeSeq.Empty
                        }

                      )
                    }: NodeSeq
                    case fullPath @ (group :: "wiki" :: _) => {
                      ((SHtml.link("/group/" + group + "/wiki" , () => {}, Text(group))):NodeSeq)
                    }
                    case _ => Text("<unknown>")
                  }},
                  "to_folder" -> parseToFolderPath(path),
                  "type" -> { resourceType match {
                    case "folder" => <img src="/images/icons/folder.gif"/>
                    case _ => <img src="/images/icons/document.gif" />
                  }}
                )
            else NodeSeq.Empty
          }
          catch {
            case e: C3ClientException => {
              logger error e.toString()
              NodeSeq.Empty
            }
          }

        })
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
      // doesn't work
      /*"query" -> AutoComplete(searchString, (current: String,limit: Int) =>
        User.currentSearchRequests.filter(_.toLowerCase.startsWith(current.toLowerCase)),
        value => processQuery(value),
        attrs = ("placeholder","Search")
      ),*/
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
      // doesn't work
      /*"search_string" -> AutoComplete("", (current: String,limit: Int) =>
        User.currentSearchRequests.filter(_.toLowerCase.startsWith(current.toLowerCase)),
        searchParam = _,
        attrs = ("placeholder","Search")
      ), */
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
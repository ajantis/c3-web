package org.aphreet.c3.snippet.groups.snippet

import net.liftweb.widgets.tablesorter.{TableSorter,Sorter, DisableSorting}
import xml.{Text, NodeSeq}
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds.{OnLoad, Script}
import net.liftweb.http.js.JsCmds

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 */
class TableSorters {
  val headers = (0, DisableSorting()) :: (1,Sorter("text")):: (2,Sorter("text")) :: (3,Sorter("usLongDate")) :: Nil
  val sortList = Nil

  val options = TableSorter.options(headers,sortList)

  def render = {
    "* *" #> ((x: NodeSeq) => x ++ Script(TableSorter.jsRender("#table-sort").cmd))
  }

//  def defaultTableSorter(xml : NodeSeq) : NodeSeq = {
//    TableSorter("#myTable")
//  }


}

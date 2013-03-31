package org.aphreet.c3.snippet.groups.snippet

import net.liftmodules.widgets.tablesorter.{TableSorter,Sorter, DisableSorting}
import xml.{Text, NodeSeq}

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 */
class TableSorters {
  val headers = (0, Sorter("text")) :: (1,Sorter("text")):: (2,Sorter("text")) :: (3,Sorter("usLongDate"))::(4, DisableSorting()) :: Nil
  val sortList = Nil

  val options = TableSorter.options(headers,sortList)

  def defaultTableSorter(xhtml: NodeSeq) : NodeSeq = {
    TableSorter("#table-sort", options)
  }


}

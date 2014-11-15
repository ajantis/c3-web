package org.aphreet.c3.util.helpers

import scala.annotation.tailrec

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
trait FSHelper {

  def transformToPathLists(fullPath: List[String]): List[List[String]] = {
    @tailrec
    def transform(acc: List[List[String]], fullPath: List[String]): List[List[String]] = {
      fullPath match {
        case x :: Nil  => acc
        case _ :: tail => transform(tail.reverse :: acc, tail)
        case Nil       => acc
      }
    }

    if (fullPath.isEmpty) Nil
    else transform(List(fullPath), fullPath.reverse)
  }

  case class FSLoc(path: List[String]) {
    def title: String = path.lastOption.getOrElse("N/A")
  }
}
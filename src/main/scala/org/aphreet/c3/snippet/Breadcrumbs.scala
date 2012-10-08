package org.aphreet.c3.snippet

import net.liftweb.util.BindHelpers._
import net.liftweb.sitemap.Loc
import net.liftweb.http.S
import xml.{NodeSeq, Text}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.util.Helpers._

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */

class Breadcrumbs {

  def breadcrumb = "*" #> {
    val breadcrumbs: List[Loc[_]] =
      for {
        currentLoc <- S.location.toList
        loc <- currentLoc.breadCrumbs
      } yield loc

    val currentLoc = breadcrumbs.lastOption

    def setActiveClass(loc: Loc[_]) = {
      if(loc == currentLoc.get)
        ".link [class+]" #> "active" & // attach class
        ".divider" #> NodeSeq.Empty // remove last divider
      else
        ".link [class!]" #> "active" // remove class
    }

    def getLocLink[T](loc: Loc[T])(implicit manifest : Manifest[T]): NodeSeq = {

      def compareLocParamTypes[A, B](l1: Loc[A],l2: Loc[B])(implicit m1 : Manifest[A], m2 : Manifest[B]): Boolean = {
        m1 == m2
      }

      def findContextValueForLoc(loc: Loc[T]): Box[T] = {
        breadcrumbs.dropWhile(loc != _) match {
          case Nil => Empty
          case xs => xs.tail.filter(l => !l.currentValue.isEmpty && compareLocParamTypes(loc, l)).headOption.map(_.currentValue.open_!.asInstanceOf[T])
        }
      }

      val linkBox: Option[NodeSeq] = loc.currentValue match {
        case Full(v) => loc.createLink(v)
        case _ =>
          findContextValueForLoc(loc).choice((v: T) => loc.createLink(v))(loc.createDefaultLink)
      }

      linkBox.getOrElse(Text("#"))
    }

    "li *" #> breadcrumbs.map {
      loc =>
        ".link *" #> loc.title &
        ".link [href]" #> getLocLink(loc) &
        setActiveClass(loc)
    }
  }

}
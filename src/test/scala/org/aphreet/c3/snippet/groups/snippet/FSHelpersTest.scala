package org.aphreet.c3.snippet.groups.snippet

import org.specs.Specification

/**
 * @author Dmitry Ivanov (mailto: id.ajantis@gmail.com)
 *         iFunSoftware
 */
object FSHelpersTest extends Specification {

  val helpers = new FSHelpers {}

  import helpers._

  "'FSHelpers.transformToPathLists' should build a proper List of paths list of string" in {
    val result = transformToPathLists(List("directory1", "directory2", "directory3", "directory4"))

    result must be equalTo (
      List(
        List("directory1"),
        List("directory1", "directory2"),
        List("directory1", "directory2", "directory3"),
        List("directory1", "directory2", "directory3", "directory4")))
  }

  "'FSHelpers.transformToPathLists' should build return an empty list of paths for an empty list of strings" in {
    val result = transformToPathLists(Nil)

    result must be equalTo (Nil)
  }

}
package org.aphreet.c3.util.helpers

import net.liftweb.common.Box
import net.liftweb.util.Helpers._

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 */
object ByteCalculatorHelper {
  def convert(value: String): Box[String] = {
    tryo {
      val v = value.toLong
      val st = math.log(v) / math.log(2)
      if (st < 10) v.toString + " B"
      else if (st < 20) "%.1f".format(v / math.pow(2, 10)) + " KB"
      else if (st < 30) "%.1f".format(v / math.pow(2, 20)) + " MB"
      else "%.1f".format(v / math.pow(2, 30)) + " GB"
    }

  }

}

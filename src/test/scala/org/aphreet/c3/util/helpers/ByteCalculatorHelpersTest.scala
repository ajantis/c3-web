package org.aphreet.c3.util.helpers

import org.specs.Specification
import net.liftweb.common.Full

/**
 * @author Serjk (mailto: serjk91@gmail.com)
 */
object ByteCalculatorHelpersTest extends Specification {

  "'ByteCalculatorHelpers.convert' should format a byte '36' to '36 B'" in {
    val valueByte = "36"
    val result = ByteCalculatorHelpers.convert(valueByte)
    result must be equalTo(Full("36 B"))
  }

  "'ByteCalculatorHelpers.convert' should format a byte '1025' to '1,0 KB'" in {
    val valueByte = "1025"
    val result = ByteCalculatorHelpers.convert(valueByte)
    result must be equalTo(Full("1,0 KB"))
  }

  "'ByteCalculatorHelpers.convert' should format a byte '1048576' to '1,0 MB'" in {
    val valueByte = "1048576"
    val result = ByteCalculatorHelpers.convert(valueByte)
    result must be equalTo(Full("1,0 MB"))
  }


}

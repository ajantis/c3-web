package org.aphreet.c3.util.helpers

/**
 * Created with IntelliJ IDEA.
 * User: PartyLeader
 * Date: 4/27/13
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
object ConvertHelper {

  def ShortString(text: String, size: Int = 80) =
    {
      if (text == null || text.length < 100) text
      else text.substring(0, size / 2) + " ... " + text.substring(text.length - size / 2)
    }
}

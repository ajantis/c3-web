package org.aphreet.c3.service.acl

import junit.framework.TestCase
import junit.framework.Assert._
import org.aphreet.c3.util.helpers.FileSharingHelper

/**
 * Created by a-legotin on 6/4/2014.
 * @author a-legotin
 */
class HashCalculatingTest extends TestCase {

  var stringForHash: String = _
  var md5_hash: String = _

  /**
   * @define setting up variables for test
   * stringForHash - this is string for which to create md 5 hash
   * md5_hash - this is true hash
   */
  override def setUp() {
    stringForHash = "this_test_string"
    md5_hash = "28e4b56624e11258dbf58f1d892e56f4"
  }

  /**
   * @define main method.
   * Call method - hash generator from FileSharingHelper and then check results.
   * @return test result
   */
  def testMD5HashGeneration() {
    assert(md5_hash.toString == FileSharingHelper.md5Hash(stringForHash).toString)
    assertNotNull(FileSharingHelper.md5Hash(stringForHash))

  }
}
package org.aphreet.c3.lib.metadata

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
object Metadata {
  val DESCRIPTION_META = "x-c3-description"
  val TAGS_META: String = "x-c3-tags"
  val MSG_CREATOR_META: String = "x-c3-msg-creator"
  val MSG_DATE_META: String = "x-c3-msg-date"
  val S4_PROCESSED_FLAG_META = "x-s4-meta-processed"
  val OWNER_ID_META = "x-c3-web-owner"
  val GROUP_ID_META = "x-c3-web-group"
  val FS_PATH_META = "c3.ext.fs.path"
  val TAGS_SEPARATOR = ","

  val systemKeySet: Set[String] = Set(DESCRIPTION_META,TAGS_META,MSG_CREATOR_META,MSG_DATE_META,S4_PROCESSED_FLAG_META,OWNER_ID_META,GROUP_ID_META,FS_PATH_META)
  def filterSystemKey(meta:scala.collection.Map[String,String]) = {
    meta.filterNot{case (k, v) => systemKeySet.contains(k)}

  }

}
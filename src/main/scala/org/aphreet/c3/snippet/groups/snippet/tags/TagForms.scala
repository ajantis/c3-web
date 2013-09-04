package org.aphreet.c3.snippet.groups.snippet.tags

import xml.NodeSeq
import net.liftweb.http.js.{JsExp, JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds.{Script, OnLoad}
import net.liftweb.http.js.jquery.JqJsCmds
import org.aphreet.c3.lib.metadata.Metadata._
import net.liftweb.http.js.JE.{JsVar, JsRaw}
import net.liftweb.util.CssSel
import com.ifunsoftware.c3.access.fs.C3FileSystemNode
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import org.aphreet.c3.lib.metadata.Metadata._

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */
trait TagForms {

  def tagsForm(fsNode: C3FileSystemNode) = {

    def addTag(tag: String): JsCmd = {
      JsCmds.Alert("Added tag: " + tag)
    }

    def deleteTag(tag: String): JsCmd = {
      JsCmds.Alert("Deleted tag: " + tag)
    }

    "#node-tags-form" #> { (xml: NodeSeq) =>
      xml ++ Script(OnLoad(JsRaw(
        """
           $(document).ready(function() {
        	    		        excludes = function (tag) {
                            // return false if this tagger does *not* exclude
                            // -> returns true if tagger should exclude this tag
                            // --> this will exclude anything with ,
                            return (tag.indexOf(",") != -1) || (tag.trim() === '');
                          };
                          whenAddingTag = function (tag) {
                          """ + SHtml.ajaxCall(JsVar("tag"), (s: String) => addTag(s))._2.cmd.toJsCmd + """
                          };
                          tagRemoved = function (tag) {
                         """ + SHtml.ajaxCall(JsVar("tag"), (s: String) => deleteTag(s))._2.cmd.toJsCmd + """
                          };
                          $('#node-tags').tags( {
                            tagData : [""" + getTags(fsNode).map(t => "\"" + t + "\"").mkString(",") + """],
                            tagClass : 'btn-info',
                            excludes : excludes,
                            whenAddingTag : whenAddingTag,
                            tagRemoved : tagRemoved
                          });
        });""").cmd))
    }
  }

  private def getTags(node: C3FileSystemNode): List[String] =
      node.metadata.get(TAGS_META).map(_.split(TAGS_SEPARATOR).toList).getOrElse(Nil)

}
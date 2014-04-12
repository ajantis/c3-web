/**
 * Copyright (c) 2011, Dmitry Ivanov
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following disclaimer
 * in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the IFMO nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.aphreet.c3.model

import net.liftweb.mapper._
import net.liftweb.util.FieldError
import xml.{ NodeSeq, Text }
import net.liftweb.common._
import net.liftweb.http.SHtml
import org.aphreet.c3.apiaccess.C3
import net.liftweb.util.Helpers._
import com.ifunsoftware.c3.access.fs.{ C3File, C3FileSystemNode }
import org.aphreet.c3.lib.metadata.Metadata._
import net.liftweb.mapper.Cmp
import org.aphreet.c3.lib.DependencyFactory
import org.aphreet.c3.lib.DependencyFactory._
import net.liftweb.mapper.Cmp
import com.ifunsoftware.c3.access.C3System
import DependencyFactory._
import net.liftweb.common.Full
import net.liftweb.mapper.Cmp
import scala.collection.immutable.Stream

class Group extends LongKeyedMapper[Group] with IdPK with ManyToMany {

  thisgroup =>

  def getSingleton = Group
  private val c3 = inject[C3System].open_!

  object owner extends MappedLongForeignKey(this, User) {
    override def toForm = Box.!!(SHtml.selectObj[User](User.findAll().map(user => (user, user.email.is)), User.currentUser, usr => thisgroup.owner(usr)))
  }

  object users extends MappedManyToMany(UserGroup, UserGroup.group, UserGroup.user, User)

  object name extends MappedString(this, 64) {
    override def validations = nonEmpty _ :: isUnique _ :: Nil

    def isUnique(s: String): List[FieldError] = {
      if (!Group.find(Cmp(Group.name, OprEnum.Eql, Full(s.toLowerCase), None, Full("LOWER"))).isEmpty)
        List(FieldError(this, "Group with name " + s + " already exists"))
      else Nil
    }
    private def nonEmpty(s: String) =
      if (s.isEmpty) List(FieldError(this, "Groups's name cannot be empty"))
      else Nil

  }
  object description extends MappedText(this)

  object tags extends MappedText(this)

  object isOpen extends MappedBoolean(this) {
    override def defaultValue = false
  }

  object isApproved extends MappedBoolean(this) {
    override def defaultValue = false
  }

  def getChildren: List[C3FileSystemNode] = getChildren("")

  def getChildren(directory: String): List[C3FileSystemNode] =
    c3.getFile(baseFilePath + directory).asDirectory.children()

  def getFile(path: String): Box[C3FileSystemNode] = tryo {
    c3.getFile(baseFilePath + path)
  }

  def getTags = {
    try {
      c3.getFile(baseGroupDirectory).metadata.get(TAGS_META).map(_.split(TAGS_SEPARATOR).toList).getOrElse(Nil)
    } catch {
      case x: Exception => Nil
    }

  }
  def getDescription = {
    try {
      c3.getFile(baseGroupDirectory).metadata.get(DESCRIPTION_META).getOrElse("")
    } catch {
      case x: Exception => ""
    }

  }

  def getGroupC3: Box[C3FileSystemNode] = {
    tryo(c3.getFile(baseGroupDirectory))
  }

  override def delete_! : Boolean = {
    UserGroup.findAll(By(UserGroup.group, this)).foreach(_.delete_!)
    Category.findAll(By(Category.linkedGroup, this)).foreach(_.delete_!)
    super.delete_!
  }

  def baseFilePath = "/" + this.id.is + "/files"

  def baseGroupDirectory = "/" + this.id.is

  def createLink: String = "/groups/" + id.is
}

object Group extends Group with LongKeyedMetaMapper[Group] {

  override def dbTableName = "groups"

  override def fieldOrder = name :: Nil

  def findByName(name: String) = find(By(Group.name, name))

  def findOpenGroups = findAll(By(Group.isOpen, true))

}
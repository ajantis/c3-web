package org.aphreet.c3.util.helpers

import java.text.SimpleDateFormat

import net.liftweb.util.TimeHelpers._
import org.aphreet.c3.model.Group

/**
 * @author nicl.nno
 * @define shall move file to another directory within group
 * @version 1.0
 */
object FileTransferHelper {
  var draggableResourceName = "";

  def moveSelectedResource(group: Group, currentAddress: String, targetFileFolder: String, moveBack: Boolean) = {
    if (draggableResourceName != "") {
      val movableFile = group.getFile(currentAddress + draggableResourceName)
      movableFile.foreach {
        f =>
          var newPath = (f.fullname.split("/").toList.init ::: targetFileFolder + "/" + draggableResourceName :: Nil).mkString("", "/", "")
          if (moveBack) newPath = targetFileFolder.substring(targetFileFolder.drop(1).indexOf('/') + 1, targetFileFolder.length) + "/" + draggableResourceName
          f.move(newPath)
      }
    }
  }

  def moveToTrashCan(name: String, group: Group, currentAddress: String, IsAddressContainsName: Boolean) = {
    if (name != "") {
      val fileName = if (IsAddressContainsName) currentAddress else currentAddress + name;
      val movableFile = group.getFile(fileName)
      val customDateFormatter = new SimpleDateFormat("dd_MM_yyyy_HH-mm-SS")
      movableFile.foreach {
        f =>
          val newName = name + "_" + customDateFormatter.format(now.getTime());
          f.move(group.baseFilePath + group.trashCanDirectory + newName)
      }
    }
  }

  def saveDraggableResourceName(resourceName: String) = {
    draggableResourceName = resourceName
  }
}

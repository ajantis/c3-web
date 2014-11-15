package org.aphreet.c3.util.helpers

import org.aphreet.c3.model.Group

/**
 * @author nicl.nno
 * @define shall move file to another directory within group
 * @version 1.0
 */
object FileTransferHelper {
  var draggableResourceName = "";

  def moveSelectedFile(group: Group, currentAddress: String, targetFileFolder: String, moveBack: Boolean) = {
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

  def saveDraggableResourceName(resourceName: String) = {
    draggableResourceName = resourceName
  }
}

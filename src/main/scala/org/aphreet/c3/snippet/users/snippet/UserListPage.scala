package org.aphreet.c3.snippet.users.snippet

import net.liftweb.util.BindHelpers._
import org.aphreet.c3.model.{UserGroup, Group, User}
import net.liftweb.http.SHtml
import xml.NodeSeq
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.mapper.By

/**
 * Copyright iFunSoftware 2011
 * @author Dmitry Ivanov
 */

class UserListPage extends UserHelpers{
  def list = {
    val users = User.findAll(By(User.enabled,true))
    ".user " #> users.map {user => {
      if (User.currentUser.open_!.superUser.is && !user.superUser.is){
        def deleteUser():JsCmd = {
          user.enabled.set(false)
          user.save
          if (!user.enabled.is )
          {
            JsCmds.Replace(user.id.is.toString, NodeSeq.Empty)
          }
          else JsCmds.Alert("User is not removed! You don't have administrator rights! Please check logs for details")
        }
        ".user [id]" #> user.id.is &
        ".user *" #>
          ((n: NodeSeq) =>  SHtml.ajaxForm(
            (toCssBindings(user)&
              ".enabled *" #> (if(user.enabled.is) "Yes" else "No") &
              ".deluser *" #> SHtml.memoize(f => f ++ SHtml.hidden(deleteUser _))).apply(n)
          ))

      }
      else
      {
        toCssBindings(user)&
        ".enabled *" #> NodeSeq.Empty &
        ".deluser *" #> NodeSeq.Empty
      }
    }

    }
  }
}
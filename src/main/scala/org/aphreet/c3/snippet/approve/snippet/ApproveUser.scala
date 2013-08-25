package org.aphreet.c3.snippet.approve.snippet

import net.liftweb.util.Helpers._
import org.aphreet.c3.model.User
import net.liftweb.mapper.By
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JsCmd}
import scala.xml.NodeSeq
import org.aphreet.c3.snippet.LiftMessages

/**
 * @author Koyushev Sergey (mailto: serjk91@gmail.com)
 */
class ApproveUser {

  def render = {
    val listUser=User.findAll(By(User.enabled,false))
    ".list_user" #> listUser.map(user=>{

      def approveUser():JsCmd={
        user.enabled(true).save
        JsCmds.Replace(user.email.toString(), NodeSeq.Empty)&
        LiftMessages.ajaxNotice("User "+user.niceName +" is approve")
      }
      ".list_user [id]" #> user.email &
      ".first_name *" #> user.firstName &
      ".last_name *" #> user.lastName &
      ".email *" #> user.email &
      ".approve_user [onclick]"#> SHtml.ajaxInvoke(()=>approveUser())

    })

  }

}

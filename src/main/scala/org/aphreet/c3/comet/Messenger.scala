package org.aphreet.c3.comet

import org.aphreet.c3.model.{User, Message}
import net.liftweb.http.{SHtml, CometListener, CometActor}
import org.aphreet.c3.notifications.{DeleteAll, MessageServer}
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.util.TimeHelpers.now

/**
 * Copyright (c) 2011, Dmitry Ivanov
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
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
 
 
 
class Messenger extends CometActor with CometListener {

   val welcomeMsg = Message.create.text("Welcome!").author(ServerUser)

   private var msgs: List[Message] = List(welcomeMsg)

   def registerWith = MessageServer

   override def lowPriority = {
     case m : List[Message] => {
       msgs = m
       reRender(false)

     }
   }

   def render = {
    <h3 class="alt"><a href="javascript:showDiv('messenger')"><img src="/images/comment_add_48.png" style="position: relative; top: 8px;" width="32" height="32" align="middle"/></a> Messenger</h3>
    <div id="messenger" style="word-wrap: break-word;width: 230px;">
    <table class="messenger" cellspacing="0">
    <tr style="width: 230px;">
    <td class="messengerHd">
    {<lift:form>
      {
        SHtml.text("", s =>
            {
              val msg = Message.create; msg.text(s).author(User.currentUser openOr Guest).dateCreated(now); MessageServer ! msg
            }
          )
      }
      <input type="image" src="/images/submit-ic.PNG"/>
      </lift:form>
    }
    </td>
    </tr>
    {
      msgs.map(m => {<tr style="width: 230px;"><td class="messengerBod" style="width: 230px;" ><p><span class="rightDate">{m.dateCreatedFormatted}</span><br/><b>{m.authorName}</b>:<br/><span style="word-break: break-word;">{m.text.is}</span></p></td></tr>})
    }
    </table>
    {<lift:form>{SHtml.ajaxButton("Clear",{ () => {if(User.loggedIn_?){MessageServer ! DeleteAll }; SetHtml("TODO",{<hr4></hr4>}) }},("class" -> "button"))}</lift:form>}
    </div>
  }

}

object ServerUser extends User {
  this.firstName("C3 client")
}

object Guest extends User {
  this.firstName("Guest")
}
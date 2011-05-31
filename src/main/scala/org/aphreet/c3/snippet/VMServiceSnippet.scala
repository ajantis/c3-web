package org.aphreet.c3.snippet

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

import net.liftweb.util.Helpers._
import xml.{Text, NodeSeq}
import org.aphreet.c3.plabaccess.PlabClient
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.common.{Full, Empty, Box}
import net.liftweb.util.TimeHelpers
import org.apache.commons.httpclient.util.URIUtil
import com.vmware.vim25.mo.{Network, HostSystem, VirtualMachine}
import net.liftweb.http.{SessionVar, S, RequestVar, SHtml}
import net.liftweb.http.js.JsCmds.{RedirectTo, SetHtml, SetValById, Alert}

class VMServiceSnippet {

  private object selectedVMs extends RequestVar[scala.collection.mutable.HashSet[VirtualMachine]](scala.collection.mutable.HashSet())
  private object currentCommand extends RequestVar[Command](RebootSelectedVMs)

  abstract class Command {
    val name: String
  }
  case object RebootSelectedVMs extends Command {
    val name = "Reboot VMs"
  }
  case object StartupSelectedGuests extends Command {
    val name = "Startup Guests"
  }
  case object ShutdownSelectedGuests extends Command {
    val name = "Shutdown Guests"
  }

  val cmdList: List[Command] = List(RebootSelectedVMs,StartupSelectedGuests,ShutdownSelectedGuests)

  def viewVMs(html: NodeSeq): NodeSeq = {

    def doCommand(cmd: Command): JsCmd = {
      cmd match {
        case RebootSelectedVMs => rebootSelected
        case StartupSelectedGuests => startupSelected
        case ShutdownSelectedGuests => shutdownSelected
        case _ => Alert("Error: unknown action.")
      }
    }

    def shutdownSelected: JsCmd = {
          for (vm <- selectedVMs){
            if(vm.getRuntime.getPowerState.name == "poweredOn")
              vm.powerOffVM_Task()
          }
          Alert("Selected VMs have been powered off: "+selectedVMs.map(_.getName).mkString(", ")) & JsCmds.RedirectTo("")
        }


    def startupSelected: JsCmd = {
      for (vm <- selectedVMs){
        if(vm.getRuntime.getPowerState.name == "poweredOff")
          vm.powerOnVM_Task(null)
      }
      Alert("Selected VMs have been started: "+selectedVMs.map(_.getName).mkString(", ")) & JsCmds.RedirectTo("")
    }

    def rebootSelected: JsCmd = {
      for (vm <- selectedVMs){
        if(vm.getRuntime.getPowerState.name != "poweredOff")
          vm.rebootGuest()
      }
      Alert("Selected VMs have been rebooted: "+selectedVMs.map(_.getName).mkString(", ")) & JsCmds.RedirectTo("")
    }


    bind("vms", html,
      "list" -> ( (ns: NodeSeq) =>
         PlabClient().getVMs() flatMap( vm => bind("vm",ns,
           "name" -> <a href={"/vmservice/vm/"+ URIUtil.encodeQuery(vm.getName,"UTF-8")}>{vm.getName}</a>,
           "uptime" ->( if(vmIsOnline(vm)) ( (TimeHelpers.now.getTime - vm.getRuntime.getBootTime.getTimeInMillis)/ 1000 / 60).minutes.toString  else "N/A" ),
             //Box[Integer](vm.getRuntime quickStats.getUptimeSeconds).map(i => i.toString).openOr("unknown"),
           "vmware_tools" -> ( vm.getGuest.getToolsRunningStatus match {
             case _ => <img src="/images/icons/" /> // TODO
           } ),
           "memory_usage" -> {
             vm.getSummary.getQuickStats.getGuestMemoryUsage match {
               case null => "0"
               case mu => mu.toString
             }
           },
           "power_state" -> ( vm.getRuntime.getPowerState.name match {
             case "poweredOn" => <img src="/images/icons/circle_green.png" width="24" height="24" />
             case "poweredOff" => <img src="/images/icons/circle_red.png" width="24" height="24" />
             case _ => <img src="/images/icons/circle_orange.png" width="24" height="24" />
           } ),
           "select" -> SHtml.ajaxCheckbox(false, (checked) => {
               if(checked) selectedVMs += vm
               else selectedVMs -= vm
               JsCmds.Noop
           }),
           "reboot" -> SHtml.link("/index",() => vm.rebootGuest,Text("reboot"))
         )): NodeSeq
      ),
      "select_action" -> ( (ns: NodeSeq) =>
        SHtml.ajaxSelectObj[Command]( cmdList.map(cmd => (cmd, cmd.name) ), Full(RebootSelectedVMs), (c:Command) => { currentCommand.set(c); JsCmds.Noop } )
      ),
      "do_action" -> ( (ns: NodeSeq) => SHtml.ajaxButton(ns, () => doCommand(currentCommand.is) )  )
    )
  }


  def hostInfo(html: NodeSeq): NodeSeq = {

    PlabClient().getHost match {
      case Full(host: HostSystem) => {
        bind("host", html,
          "name" -> host.getName,
          "memory_available" -> (host.getHardware.getMemorySize / 1024 / 1024 ).toString,
          "server_model" -> host.getHardware.getSystemInfo.getModel,
          "vendor" -> host.getHardware.getSystemInfo.getVendor,
          "cpu_info" -> ( (ns: NodeSeq) =>
            bind("cpu", ns,
             "cores" -> host.getHardware.getCpuInfo.getNumCpuCores,
             "hz" -> host.getHardware.getCpuInfo.getHz / 1024 / 1024,
             "threads" -> host.getHardware.getCpuInfo.getNumCpuThreads
            )
          )
        )
      }
      case _ => {
        S error "Host "+ PlabClient().hostname+" wasn't found"
        NodeSeq.Empty
      }
    }

  }

  private object currentFlot extends SessionVar[String]("mem_usage")

  def vmOverview(html: NodeSeq): NodeSeq = {

    def renderVMPowerStatus(vm: VirtualMachine):NodeSeq =
      ( vm.getRuntime.getPowerState.name match {
                 case "poweredOn" => <img src="/images/icons/circle_green.png" width="24" height="24" />
                 case "poweredOff" => <img src="/images/icons/circle_red.png" width="24" height="24" />
                 case _ => <img src="/images/icons/circle_orange.png" width="24" height="24" />
      })


    S.param("vmName") match {
      case Full(vmName: String) => {
        PlabClient().getVMByName(vmName) match {
          case Some(vm) => {

            def chooseGraph(ns: NodeSeq): NodeSeq = currentFlot.get match {
              case "active_mem" => new FlotBuilder().vmPieMemoryDetailed(ns,vm)
              case _ => new FlotBuilder().vmPieMemory(html,vm)  // by default
            }

            bind("vm",html,
               "name" -> vm.getName,
               "uptime" -> ( if(vmIsOnline(vm)) ( (TimeHelpers.now.getTime - vm.getRuntime.getBootTime.getTimeInMillis)/ 1000 / 60).minutes.toString  else "N/A" ),
               "num_cpu" -> vm.getSummary.getConfig.getNumCpu.toString,
               "memory" -> (vm.getSummary.getConfig.getMemorySizeMB.toString+" Mb"),
               "vmware_tools" -> ( vm.getGuest.getToolsRunningStatus match {
                 case "guestToolsRunning" => <img src="/images/accepted_48.png" width="24" height="24"/> // TODO
                 case _ => <img src="/images/cancel_48.png" width="24" height="24"/>
               } ),
               "memory_usage" -> {
                 vm.getSummary.getQuickStats.getGuestMemoryUsage match {
                   case null => "0"
                   case mu => mu.toString
                 }
               },
               "power_state" -> renderVMPowerStatus(vm),
               "reboot_button" -> ( (ns: NodeSeq) =>
                   SHtml.ajaxButton(ns, () => {
                      vm.rebootGuest
                      Thread.sleep(10 seconds)
                      SetHtml("vm_powerstate",renderVMPowerStatus(vm))
                    },
                   (if(vm.getRuntime.getPowerState.name!="poweredOn") ("disabled" -> "disabled") else ("enabled" -> "enabled")))
               ),
               "change_power_state" -> ( (ns: NodeSeq) =>
                 vm.getRuntime.getPowerState.name match {
                   case "poweredOn" => SHtml.ajaxButton(<img src="/images/stop_48.png" width="24" height="24"/>, () => {
                      vm.powerOffVM_Task()
                      Thread.sleep(10 seconds)
                      SetHtml("vm_powerstate",renderVMPowerStatus(vm))
                     })
                   case _ => SHtml.ajaxButton(<img src="/images/play_48.png" width="24" height="24"/>, () => {
                      vm.powerOnVM_Task(null)
                      Thread.sleep(10 seconds)
                      SetHtml("vm_powerstate",renderVMPowerStatus(vm))
                     })
                 }
               ),
               "suspend_button" -> ( (ns: NodeSeq) =>
                   SHtml.ajaxButton(ns, () => {
                      vm.suspendVM_Task
                      Thread.sleep(10 seconds)
                      SetHtml("vm_powerstate",renderVMPowerStatus(vm))
                     },
                   (if(vm.getRuntime.getPowerState.name != "poweredOn") ("disabled" -> "disabled") else ("enabled" -> "enabled") )
               )),
               "stat_chart" -> chooseGraph(html),
               "memory_pie_chart" ->( new FlotBuilder().vmPieMemory(html, vm) ),
               "memory_detailed_pie_chart" -> ( new FlotBuilder().vmPieMemoryDetailed(html,vm) ),    //
               "networks" -> ( (ns: NodeSeq) =>
                 vm.getNetworks.toSeq.flatMap( (network: Network) =>
                  bind("network", ns,
                    "name" -> network.getName,
                    "neighbours" -> ( (nss: NodeSeq) => network.getVms.toSeq.flatMap(vm => bind("vm",nss,"name" -> vm.getName) ):NodeSeq)
                  )
                ):NodeSeq ),
               "choose_graph" -> SHtml.ajaxSelect(List( "mem_usage" -> "Memory usage","active_mem" -> "Active memory info"),Full(currentFlot.get), (opt: String) => {currentFlot.set(opt); RedirectTo("")} )
            )
          }
          case _ => {
            S error "VM is not found"
            NodeSeq.Empty
          }
        }
      }
      case _ => {
        S error "VM is not defined"
        NodeSeq.Empty
      }
    }
  }


  private def vmIsOnline(vm: VirtualMachine): Boolean = {
      vm.getRuntime.getPowerState.name match {
        case "poweredOn" => true
        case _ => false
      }
  }



}
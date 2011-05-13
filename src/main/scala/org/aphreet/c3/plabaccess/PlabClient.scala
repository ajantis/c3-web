package org.aphreet.c3.plabaccess

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

import com.vmware.vim25.mo._
import java.net.URL
import net.liftweb.util.Props
import net.liftweb.common.{Full, Empty, Box, Logger}
import com.vmware.vim25.PerfMetricId

class PlabClient(val url: String,val user: String, val password: String) {

   val logger = Logger(classOf[PlabClient])

   val hostname = PlabClient.hostname

   val si = new ServiceInstance(new URL( url ), user, password, true)

   val rootFolder = si.getRootFolder

   def getVMs(): List[VirtualMachine] =
      new InventoryNavigator(rootFolder).searchManagedEntities("VirtualMachine") match {
        case null =>
          logger.error("searchManagedEntities returned null")
          List[VirtualMachine]()
        case vms => vms map(_.asInstanceOf[VirtualMachine]) toList
      }

   def getHost(): Box[HostSystem] = {
      new InventoryNavigator(rootFolder).searchManagedEntity("HostSystem", hostname) match {
        case null => Empty
        case host => Full(host.asInstanceOf[HostSystem])
      }
   }

   def getVMByName(vmName: String): Option[VirtualMachine] = getVMs.find(_.getName == vmName)

   def getPerfMetrics(vmName: String): Array[PerfMetricId] = {

      getVMByName(vmName) match {
        case Some(vm) => {
          val perfMgr = si.getPerformanceManager()

          // find out the refresh rate for the virtual machine
          val pps = perfMgr.queryPerfProviderSummary(vm)
          val refreshRate = pps.getRefreshRate().intValue()

          // retrieve all the available perf metrics for vm
          val pmis = perfMgr.queryAvailablePerfMetric(vm, null, null, refreshRate)

          pmis

        }
        case _ => {
          // vm is not found on host
          Array()
        }
      }

   }

}

object PlabClient {

 val hostname = Props.get("plab_esx_hostname").openOr("plab.cs.ifmo.ru")

 val url = Props.get("plab_api_url").openOr("https://localhost/sdk")

 val username = Props.get("admin_username").openOr("")

 val password = Props.get("admin_password").openOr("")

 def apply() : PlabClient = new PlabClient( url, username, password )

}


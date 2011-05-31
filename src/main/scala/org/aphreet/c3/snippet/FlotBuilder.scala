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
 

import net.liftweb.widgets.flot._
import xml.NodeSeq
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds.Run
import com.vmware.vim25.mo.{Network, VirtualMachine}
import net.liftweb.common.{Box, Full}

class FlotBuilder {

  def render(html: NodeSeq): NodeSeq = {

    val data_values1: List[(Double,Double)] = List( (1, 10) )
    val data_values2: List[(Double,Double)] = List( (2, 30) )
    val data_values3: List[(Double,Double)] = List( (2, 60) )
    /*for (i <- List.range (0, 140, 5))
        yield (i / 10.0, Math.sin(i / 10.0) )  */

    val data_to_plot1 = new FlotSerie() {
        override val data = data_values1
        override val label = Full("adin")
    }
    val data_to_plot2 = new FlotSerie() {
        override val data = data_values2
        override val label = Full("dva")
    }
    val data_to_plot3 = new FlotSerie() {
        override val data = data_values3
        override val label = Full("tri")
    }

    val options: FlotOptions = new FlotOptions () {
      override val series = Full( Map(
        "pie" -> JsObj( "show" -> JsTrue,
          "radius" ->  0.75,
          "tilt" -> 0.5,
          "label" -> JsObj(
            "show" -> JsTrue,
            "radius" -> 1,
            "formatter" -> JsRaw("""function(label, series){return '<div style="font-size:8pt;text-align:center;padding:2px;color:white;">'+label+'<br/>'+Math.round(series.percent)+'%</div>';}"""),
            "background" -> JsObj("opacity" -> 0.8)
          ),
          "combine" -> JsObj("color" -> Str("#000"),"threshold" -> 0.1)
        )
      ))
      override val grid = Full( new FlotGridOptions {
        override def hoverable = Full(true)
        override def clickable = Full(true)
      })
      override val legend = Full(
       new FlotLegendOptions {override def show = Full(false)} )
    }

    Flot.render("flot_area", List(data_to_plot1,data_to_plot2,data_to_plot3), options, Flot.script(html) & Run(
     """ $("#flot_area").bind("plothover", pieHover);
         $("#flot_area").bind("plotclick",pieClick);"""
    )
    )

  }

  def vmPieMemory(html: NodeSeq, vm: VirtualMachine): NodeSeq = {

    val fullMemory = vm.getSummary.getConfig.getMemorySizeMB.intValue()

    val usedMemory: Int = vm.getSummary.getQuickStats.getGuestMemoryUsage match {
        case null => 0
        case mu => (mu.intValue * 100) / fullMemory
    }

    val unusedMemory = 100 - usedMemory

    val usedMemoryData = new FlotSerie() {
        override val data: List[(Double,Double)] = List( (1, usedMemory.toDouble) )
        override val label = Full("Memory used")
    }

    val unusedMemoryData = new FlotSerie() {
        override val data: List[(Double,Double)] = List( (1, unusedMemory.toDouble) )
        override val label = Full("Memory unused")
    }

    val options: FlotOptions = new FlotOptions () {
      override val series = Full( Map(
        "pie" -> JsObj( "show" -> JsTrue,
          "radius" ->  0.75,
          "tilt" -> 0.5,
          "label" -> JsObj(
            "show" -> JsTrue,
            "radius" -> 1,
            "formatter" -> JsRaw("""function(label, series){return '<div style="font-size:8pt;text-align:center;padding:2px;color:white;">'+label+'<br/>'+Math.round(series.percent)+'%</div>';}"""),
            "background" -> JsObj("opacity" -> 0.8)
          )
          //,"combine" -> JsObj("color" -> Str("#000"),"threshold" -> 0.1)
        )
      ))
      override val grid = Full( new FlotGridOptions {
        override def hoverable = Full(true)
        override def clickable = Full(true)
      })
      override val legend = Full(
       new FlotLegendOptions {override def show = Full(false)} )
    }

    Flot.render("vm_memory_flot_area", List(usedMemoryData,unusedMemoryData), options, Flot.script(html) & Run(
     """ $("#vm_memory_flot_area").bind("plothover", pieHover);
         $("#vm_memory_flot_area").bind("plotclick",pieClick);"""
    )
    )
  }

  def vmPieMemoryDetailed(html: NodeSeq, vm: VirtualMachine): NodeSeq = {

    val usedMemory = vm.getSummary.getQuickStats.getGuestMemoryUsage match {
        case null => 0
        case mu => mu.doubleValue()
    }

    val balloonedMemory = vm.getSummary.getQuickStats.getBalloonedMemory match {
        case null => 0
        case mu => mu.doubleValue()
    }
    val swappedMemory = vm.getSummary.getQuickStats.getSwappedMemory match {
        case null => 0
        case mu => mu.doubleValue()
    }
    val sharedMemory = vm.getSummary.getQuickStats.getSharedMemory match {
        case null => 0
        case mu => mu.doubleValue()
    }

    val privateMemory =  usedMemory - balloonedMemory - swappedMemory - sharedMemory

    val balloonedMemoryData = new FlotSerie() {
        override val data: List[(Double,Double)] = List( (1, balloonedMemory) )
        override val label = Full("Ballooned memory")
    }

    val swappedMemoryData = new FlotSerie() {
        override val data: List[(Double,Double)] = List( (1, swappedMemory) )
        override val label = Full("Swapped memory")
    }

    val sharedMemoryData = new FlotSerie() {
        override val data: List[(Double,Double)] = List( (1, sharedMemory) )
        override val label = Full("Shared memory")
    }

    val privateMemoryData = new FlotSerie() {
        override val data: List[(Double,Double)] = List( (1, privateMemory) )
        override val label = Full("Private memory")
    }

    val options: FlotOptions = new FlotOptions () {
      override val series = Full( Map(
        "pie" -> JsObj( "show" -> JsTrue,
          "radius" ->  0.75,
          "tilt" -> 0.5,
          "label" -> JsObj(
            "show" -> JsTrue,
            "radius" -> 1,
            "formatter" -> JsRaw("""function(label, series){return '<div style="font-size:8pt;text-align:center;padding:2px;color:white;">'+label+'<br/>'+Math.round(series.percent)+'%</div>';}"""),
            "background" -> JsObj("opacity" -> 0.8)
          )
          //,"combine" -> JsObj("color" -> Str("#000"),"threshold" -> 0.1)
        )
      ))
      override val grid = Full( new FlotGridOptions {
        override def hoverable = Full(true)
        override def clickable = Full(true)
      })
      override val legend = Full(
       new FlotLegendOptions {override def show = Full(false)} )
    }

    Flot.render("vm_memory_flot_area", List(balloonedMemoryData,swappedMemoryData,sharedMemoryData,privateMemoryData), options, Flot.script(html) & Run(
     """ $("#vm_memory_flot_area").bind("plothover", pieHover);
         $("#vm_memory_flot_area").bind("plotclick",pieClick);"""
    )
    )
  }

  def vmMemoryStacked(html: NodeSeq, vm: VirtualMachine): NodeSeq = {

    val testMemoryData = new FlotSerie() {
        override val data: List[(Double,Double)] = List( (1, 100 ),(1, 200 ),(1, 150 ) )
        override val label = Full("Memory used")
    }

    val options: FlotOptions = new FlotOptions () {

      override val series = Full( Map(
           "stack"-> JsTrue,
            "bars" -> JsObj( "show"->true, "barWidth"-> 45000000)  ) )

      override val xaxis = Full( new FlotAxisOptions() {
           override val mode = Full("time")
      })

      override val legend = Full( new FlotLegendOptions() {
           override val container = Full("legend_area")
      })

    }

    Flot.render("vm_detailed_memory_flot_area", List(testMemoryData), options, Flot.script(html))

  }

}
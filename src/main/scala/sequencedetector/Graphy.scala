

package sequencedetector

import org.graphstream.graph.implementations.SingleGraph

import scala.io.Source

class Graphy (val name : String, val strictChecking : Boolean, val autoCreate : Boolean , val styleSheet : String ) extends SingleGraph(name, strictChecking, autoCreate){
  System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer")
  addAttribute("ui.quality")
  addAttribute("ui.antialias")
  addAttribute("ui.stylesheet", Source.fromInputStream(getClass.getResourceAsStream("style.css")).getLines.mkString("\n"))
}
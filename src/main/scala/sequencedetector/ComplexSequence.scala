package sequencedetector

import org.graphstream.graph.{Edge, Node}

import scala.io.Source
import scala.math.{ceil, log}

object ComplexSequene extends App {
  val fsm = new ComplexSequenceDetectorFSM(List( "1011001"), true, true)
  FsmViewer(fsm)
}

class ComplexSequenceDetectorFSM(val sequences: List[String], val overlap: Boolean, val moore: Boolean) {
  val end_sequences = sequences.map { x => if (moore) x else x.substring(0, x.length() - 1) }
  val comp_order = end_sequences.flatMap(end_seq => List.range(0, end_seq.length() + 1).map { i => if (i > 0) end_seq.substring(0, i) else "-" }).toSet.toSeq.sortWith(_ > _).sortWith(_.length > _.length)
  val num_ff = ceil(log(comp_order.size) / log(2)).toInt
  val state_assignment = Range(0, comp_order.size).map(i => comp_order(i) -> (1 << num_ff | (comp_order.size - i - 1)).toBinaryString.substring(1))(collection.breakOut): collection.immutable.Map[String, String]
  val sequence_table = comp_order.map(seq => seq -> Range(0, 2).map(i => comp_order.find { x => (seq + i).endsWith(x) }.getOrElse("-")).toList)(collection.breakOut): collection.mutable.Map[String, List[String]]
  if (!overlap) Range(0, end_sequences.size).map { i => sequence_table(end_sequences(i)) = if (moore) sequence_table("-") else sequence_table(end_sequences(i)).updated(sequences(i).substring(sequences(i).length() - 1).toInt, "-") }
  val state_table = comp_order.map(i => state_assignment(i) -> sequence_table(i).map(j => state_assignment(j)))(collection.breakOut): collection.immutable.Map[String, List[String]]
}

object FsmViewer {
  def apply (fsm : ComplexSequenceDetectorFSM) = { 
    val fsmGraph = new FsmGraph("Graphy")
    for (src <- fsm.comp_order; tran <- Range(0, 2)) fsmGraph.addTransition(src, fsm.sequence_table(src)(tran), tran)
    fsmGraph.setNodeAttribute("-", "ui.class", "START")
    fsm.end_sequences.map { state => fsmGraph.setNodeAttribute(state, "ui.class", "END") }
    fsmGraph.display
  }
}

class FsmGraph(override val name: String) extends Graphy(name, false, true, "style.css") {
  def setNodeAttribute(id: String, attr: String, value: String) {
    val n: Node = getNode(id)
    n.addAttribute(attr, value)
  }
  def setEdgeAttribute(id: String, attr: String, value: String) {
    val e: Edge = getEdge(id)
    e.addAttribute(attr, value)
  }
  def addTransition(src: String, dst: String, tran: Int) = {
    val e: Edge = addEdge(src + "->" + dst, src, dst, true)
    val s: Node = e.getSourceNode()
    val d: Node = e.getTargetNode()
    e.addAttribute("ui.class", if (tran == 1) "ONE" else "ZERO")
    s.addAttribute("ui.label", src)
    d.addAttribute("ui.label", dst)
  }
}
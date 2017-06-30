package com.bromberger.scalagraphs

import scala.collection.AbstractSeq
import scala.collection.immutable.SortedSet

/**
  * Created by seth on 2017-06-27.
  */

abstract class AbstractEdge(val _src: Int, val _dst: Int) {
  def toList:List[Int] = List(_src, _dst)
}

object AbstractEdge {
  implicit def ordering[A <: AbstractEdge]: Ordering[A] = new Ordering[A] {
    override def compare(x: A, y: A): Int = {
      val srcs = x._src.compareTo(y._src)
      if (srcs == 0) x._dst.compareTo(y._dst) else srcs
    }
  }
}

sealed abstract case class Edge private (src: Int, dst: Int) extends AbstractEdge(src, dst) {
  override def toString: String = "Edge (" + src + ", " + dst + ")"
}

object Edge {
  def apply(u: Int, v: Int): Edge =
    if (u > v) apply(v, u) else new Edge(u, v) {}
}

case class Arc(src:Int, dst:Int) extends AbstractEdge(src, dst) {
  override def toString: String = "Arc (" + src + ", " + dst + ")"
  def this(uv:(Int, Int)) = this(uv._1, uv._2)
}

object Arc {
  def apply(uv:(Int, Int)):Arc = new Arc(uv._1, uv._2)
}

case class ScalaGraph(edgeSet:SortedSet[Edge]=SortedSet[Edge]()) {
  import ScalaGraph._
  lazy val vertexSet:Set[Int] = (srcVertices ++ dstVertices).toSet
  val nv = edgeSet.flatMap(e => e.toList).toSet.size
  val ne = edgeSet.size

  override def toString: String = "ScalaGraph {" + nv + ", " + ne + "}"

  def addEdge(e:Edge): ScalaGraph = addEdges(List[Edge](e))
  def addEdge(u:Int, v:Int): ScalaGraph = addEdge(Edge(u, v))
  def addEdges(newedges:Seq[Edge]): ScalaGraph = ScalaGraph(edgeSet ++ newedges)
  def addEdges(newedges:Iterator[Edge]): ScalaGraph = ScalaGraph(edgeSet ++ newedges)
  def hasEdge(e:Edge) = edgeSet.contains(e)
  def vertices:Iterator[Int] = vertexSet.toIterator
  def srcVertices:Iterator[Int] = edgeSet.map(e=>e.src).toIterator
  def dstVertices:Iterator[Int] = edgeSet.map(e=>e.dst).toIterator
  def outEdges(u:Int):Iterator[Edge] = edges.par.filter(e => e.src == u)
  def inEdges(v:Int):Iterator[Edge] = edges.par.filter(e => e.dst == v)
  def outNeighbors(u:Int):Iterator[Int] = outEdges(u).map(e => e.dst)
  def inNeighbors(v:Int):Iterator[Int] = inEdges(v).map(e => e.src)
  def outDegree(v:Int):Int = outEdges(v).size
  def inDegree(u:Int):Int = inEdges(u).size
  def outDegree():Map[Int, Int] = srcVertices.map(v => v->outDegree(v)).toMap
  def inDegree():Map[Int, Int] = dstVertices.map(v => v->inDegree(v)).toMap
  def edges():Iterator[Edge] = edgeSet.toIterator
  def isEmpty():Boolean = edgeSet.isEmpty
  def save(fname:String):Int = {
    import java.io._
    val s = StringBuilder.newBuilder
    edges.foreach(e => s.append(e.src + ", " + e.dst + "\n"))
    val pw = new PrintWriter(new File(fname))
    pw.write(s.toString)
    pw.close()
    s.length
  }

  def subgraph(l:List[Int]) = {
    val s = l.toSet
    val newedges = edgeSet.filter(e => s.contains(e.src) && s.contains(e.dst))
    ScalaGraph(newedges)
  }
}

object ScalaGraph {
  private def getFileExt(s:String):String = s.split("\\.").last
  def apply(l:AbstractSeq[(Int, Int)]):ScalaGraph = {
    val edgeList = l.map(sd => Edge(sd._1, sd._2))
    ScalaGraph(edgeList.to[SortedSet])
  }

  def apply(i:Iterator[Edge]):ScalaGraph = ScalaGraph(i.to[SortedSet])

  def apply(fname:String):ScalaGraph = fname match {
    case s:String if getFileExt(s) == "csv" => {
      val bufferedSource = io.Source.fromFile(fname)
      val edgeList = bufferedSource.getLines()
        .map(l => l.split(",", 2).map(_.trim))
        .map(s => Edge(s(0).toInt, s(1).toInt))
      ScalaGraph(edgeList)
    }
    case _ => throw new NoSuchMethodException("File format not recognized")
  }
}

object ScalaGraphs {
  def main(args: Array[String]): Unit = {
    val a1 = Arc(1, 2)
    val g = ScalaGraph()
    println("g = " + g)
    val h = ScalaGraph(SortedSet(Edge(4,2), Edge(1,3), Edge(4,1)))
    println("h = " + h)
    println(h.edgeSet)
    val j = h.addEdges(List(Edge(0,4), Edge(2,1), Edge(2,2), Edge(1,3), Edge(2,1))).addEdge(400,3)
    println("j = " + j)
    println(j.edges)
    j.edges.foreach(println)
    println(j.inDegree)
    println(j.outDegree(4))
    val k = ScalaGraph("/tmp/edgelist.csv")
    println("k = " + k)
    println(k.edges)
    k.edges.foreach(println)
    println(k.inDegree)
    println(k.outDegree(4))

  }
}

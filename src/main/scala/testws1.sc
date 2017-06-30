import com.bromberger.scalagraphs._
import scala.collection.immutable.SortedSet

val edgelist = List(
  (1, 2),
  (2, 3),
    (3, 4),
    (4, 5),
    (5,6),
    (6,7),
    (7, 8),
    (6, 1)
)
val g = ScalaGraph(edgelist)
g.save("/tmp/foo.csv")

val h = ScalaGraph("/tmp/foo.csv")

g == h

val s = g.subgraph(List(1,2,3,4,6))
s.edges.foreach(println)

edgelist.flatMap(e=> List(e._1, e._2)).to[SortedSet]

val edgel = List(Edge (1, 3), Edge (4, 1), Edge (4, 2))

edgel.map(e => e.toList).toSet

val e1 = Edge(1,2)
val e2 = Edge(2,1)
val e3 = Edge(2,1)
e1 == e2
e2 == e3

val a1 = Arc(1,2)
val a2 = Arc(2,1)

val i = g.edges()



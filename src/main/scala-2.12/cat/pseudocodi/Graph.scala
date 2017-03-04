package cat.pseudocodi

import scalafx.beans.property.BooleanProperty

case class Graph(nodes: Seq[Node], edges: Seq[Edge])

object Graph {

  def apply(): Graph = {
    val excluded = List(Rect(0, 0, 1), Rect(0, 5, 1), Rect(5, 0, 1), Rect(5, 5, 1))
    val nodes = for (x <- 0 until 7; y <- 0 until 7 if !excluded.exists(r => r.intersects(x, y)))
      yield Node(x, y, empty = BooleanProperty(x == 3 && y == 3))
    val edges = nodes.flatMap(n => adjacentNodes(nodes, n).map(neighbor => Edge(n, neighbor)))
    Graph(nodes, removeDuplicates(edges))
  }

  def adjacentNodes(nodes: Seq[Node], n: Node): Seq[Node] = {
    nodes.filter(node => n.isNeighbor(node))
  }

  def removeDuplicates(edges: Seq[Edge]): Seq[Edge] = {
    def doIt(acc: Seq[Edge], res: Seq[Edge]): Seq[Edge] = {
      if (acc.isEmpty) res
      else {
        if (res.exists(edge => acc.head.a == edge.a && acc.head.b == edge.b || acc.head.a == edge.b && acc.head.b == edge.a)) {
          doIt(acc.tail, res)
        } else {
          doIt(acc.tail, res :+ acc.head)
        }
      }
    }

    doIt(edges, List())
  }
}

case class Node(x: Int, y: Int, empty: BooleanProperty = BooleanProperty(false), selected: BooleanProperty = BooleanProperty(false)) {
  def isNeighbor(other: Node): Boolean = {
    other.x == x + 1 && other.y == y ||
      other.x == x - 1 && other.y == y ||
      other.x == x && other.y == y + 1 ||
      other.x == x && other.y == y - 1
  }
}

case class Edge(a: Node, b: Node)

case class Rect(x: Int, y: Int, len: Int) {
  def intersects(x: Int, y: Int): Boolean =
    (x >= this.x && x <= this.x + len) && (y >= this.y && y <= this.y + len)
}
package cat.pseudocodi

case class BoardGraph(nodes: Seq[Node], edges: Seq[Edge])

object BoardGraph {

  def apply(): BoardGraph = {
    val excluded = List(Rect(0, 0, 1), Rect(0, 5, 1), Rect(5, 0, 1), Rect(5, 5, 1))
    val nodes = for (x <- 0 until 7; y <- 0 until 7 if !excluded.exists(r => r.intersects(x, y)))
      yield Node(x, y)
    val edges = nodes.flatMap(n => adjacent(nodes, n).map(neighbor => Edge(n, neighbor)))
    BoardGraph(nodes, removeDuplicates(edges))
  }

  def adjacent(nodes: Seq[Node], n: Node): Seq[Node] = {
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

  def validMovesFor(source: Node, remainingNodes: Seq[Node], emptyNodes: Seq[Node]): Seq[Move] = {
    val neighbors = adjacent(remainingNodes, source)
    val validMoves: Seq[Move] =
      neighbors
        .flatMap(neighbor => {
          adjacent(emptyNodes, neighbor)
            .filter(n => n.isOrthogonal(source))
            .map(target => Move(source, target, neighbor, (source +: neighbor +: emptyNodes).filter(p => p != target).distinct))
        })
    validMoves
  }

  def solution(): Seq[Move] = {
    val graph = BoardGraph()

    def doIt(frontier: Seq[Seq[Move]]): Seq[Move] = {
      val moves: Seq[Move] = frontier.head
      if (moves.nonEmpty && moves.head.emptyNodes.length == graph.nodes.length - 1) {
        moves.reverse
      } else {
        val emptyNodes = moves.head.emptyNodes
        val remaining = graph.nodes.diff(emptyNodes)
        val validMoves = remaining.flatMap(n => validMovesFor(n, remaining, emptyNodes))
        if (validMoves.isEmpty) {
          doIt(frontier.tail)
        } else {
          val newValidMoves = validMoves.map(move => move +: moves)
          val newFrontier = newValidMoves ++ frontier.tail
          doIt(newFrontier)
        }
      }
    }

    val src = Node(3, 1)
    val removed = Node(3, 2)
    val target = Node(3, 3)
    val emptyNodes = List(removed, src)
    doIt(List(List(Move(src, target, removed, emptyNodes))))
  }
}

case class Node(x: Int, y: Int) {
  def isNeighbor(other: Node): Boolean = {
    other.x == x + 1 && other.y == y ||
      other.x == x - 1 && other.y == y ||
      other.x == x && other.y == y + 1 ||
      other.x == x && other.y == y - 1
  }

  def isOrthogonal(other: Node): Boolean = other match {
    case Node(a, b) if x == a || y == b => true
    case _ => false
  }
}

case class Edge(a: Node, b: Node)

case class Move(source: Node, target: Node, removed: Node, emptyNodes: Seq[Node])

case class Rect(x: Int, y: Int, len: Int) {
  def intersects(x: Int, y: Int): Boolean =
    (x >= this.x && x <= this.x + len) && (y >= this.y && y <= this.y + len)
}
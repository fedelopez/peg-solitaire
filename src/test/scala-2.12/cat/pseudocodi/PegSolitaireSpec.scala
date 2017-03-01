package cat.pseudocodi

import org.scalatest.FunSpec

class PegSolitaireSpec extends FunSpec with PegSolitaire {

  val graph: Graph = GraphFactory.graph()

  describe("Graph Factory") {
    it("should initialise a graph with 33 nodes and 52 edges") {
      assert(graph.nodes.length === 33)
      assert(graph.edges.length === 52)
    }

    it("should initialise a graph with an empty peg in the middle") {
      val actual = graph.nodes.filter(n => n.empty())
      assert(actual.size === 1)
      assert(actual.head.x === 3)
      assert(actual.head.y === 3)
    }

    it("should create every node connected to the right, left, up and down immediate neighbors") {
      graph.nodes.foreach(node => {
        val neighbors: Seq[Node] = graph.edges.filter(p => p.a == node || p.b == node).map(n => if (n.a != node) n.a else n.b)
        neighbors.foreach(neighbor => assert(node.isNeighbor(neighbor)))
      })
    }
  }
}
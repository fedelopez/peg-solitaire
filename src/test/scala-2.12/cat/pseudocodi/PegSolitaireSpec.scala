package cat.pseudocodi

import org.scalatest.FunSpec

class PegSolitaireSpec extends FunSpec with PegSolitaire {

  describe("Graph") {
    it("should initialise a graph with 33 nodes and 52 edges") {
      assert(graph.nodes.length === 33)
      assert(graph.edges.length === 52)
    }

    it("should create every node connected to the right, left, up and down immediate neighbors") {
      graph.nodes.foreach(node => {
        val neighbors: Seq[Node] = graph.edges.filter(p => p.a == node || p.b == node).map(n => if (n.a != node) n.a else n.b)
        neighbors.foreach(neighbor => assert(node.isNeighbor(neighbor)))
      })
    }

    it("should empty the board leaving only one peg") {
      val solution: Seq[Move] = BoardGraph.solution()
      val pegs = graph.nodes.map(n => Peg(n))
      solution.foreach(move => {
        pegs.find(p => p.node == move.source).foreach(p => p.empty() = true)
        pegs.find(p => p.node == move.removed).foreach(p => p.empty() = true)
        pegs.find(p => p.node == move.target).foreach(p => p.empty() = false)
      })
      assert(pegs.count(p => p.empty()) === 1)
    }
  }
}
package cat.pseudocodi

import org.scalatest.FunSpec

class PegSolitaireSpec extends FunSpec {

  describe("Graph Factory") {
    it("should initialise a graph with 33 nodes and 52 edges") {
      val graph = GraphFactory.graph()
      assert(graph.nodes.length === 33)
      assert(graph.edges.length === 52)
    }

    it("should initialise a graph with an empty peg") {
      //todo
    }
  }
}


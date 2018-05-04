package cat.pseudocodi

import org.scalatest.FunSpec

class BoardGridSpec extends FunSpec with BoardGrid {

  describe("Graph") {
    it("should empty the board leaving only one peg") {
      val path: Seq[Board] = solution()
      assert(path.head.flatten.count(hole => hole == Peg) === 1)
      assert(path.reverse.head.flatten.count(hole => hole == Peg) === 32)
    }

    it("should encode the initial board") {
      val board = initialise()
      val encoded = encode(board)
      assert(encoded === "PPPPPPPPPPPPPPPPEPPPPPPPPPPPPPPPP")
    }
  }
}
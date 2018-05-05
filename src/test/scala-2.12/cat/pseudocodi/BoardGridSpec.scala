package cat.pseudocodi

import cat.pseudocodi.BoardGrid.{Board, Peg}
import org.scalatest.FunSpec

class BoardGridSpec extends FunSpec {

  describe("Graph") {
    it("should empty the board leaving only one peg") {
      val path: Seq[Board] = BoardGrid.solution()
      assert(path.head.flatten.count(hole => hole == Peg) === 1)
      assert(path.reverse.head.flatten.count(hole => hole == Peg) === 32)
    }

    it("should encode the initial board") {
      val board = BoardGrid.initialise()
      val encoded = BoardGrid.encode(board)
      assert(encoded === "PPPPPPPPPPPPPPPPEPPPPPPPPPPPPPPPP")
    }
  }
}
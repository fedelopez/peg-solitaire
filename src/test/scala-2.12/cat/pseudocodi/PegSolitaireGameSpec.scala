package cat.pseudocodi

import org.scalatest.FunSpec

class PegSolitaireGameSpec extends FunSpec with PegSolitaireGame {

  describe("PegSolitaireGame") {
    it("is not complete when the board is initialised") {
      assert(newBoard().isComplete === false)
    }

    it("is not complete when the board is ongoing") {
      val board = newBoard()
      board.cells(4)(3) = Empty
      assert(board.isComplete === false)
    }

    it("is complete when the board contains only one peg") {
      val board = newBoard()
      for (i <- board.cells.indices; j <- board.cells(i).indices) {
        if (board.cells(i)(j) == Filled) board.cells(i)(j) = Empty
      }
      board.cells(3)(3) = Filled
      assert(board.isComplete === true)
    }
  }
}
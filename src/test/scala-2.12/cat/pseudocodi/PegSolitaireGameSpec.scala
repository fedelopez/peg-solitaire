package cat.pseudocodi

import org.scalatest.FunSpec

class PegSolitaireGameSpec extends FunSpec with PegSolitaireGame {

  describe("PegSolitaireGame") {
    it("is not complete when the board is initialised") {
      assert(newBoard().isComplete === false)
    }
  }
}
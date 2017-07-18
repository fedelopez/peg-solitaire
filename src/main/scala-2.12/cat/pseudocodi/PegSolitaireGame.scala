package cat.pseudocodi

trait PegSolitaireGame {

  type State = Int
  type Board = Array[Array[State]]

  val Empty: State = 0
  val Filled: State = 1
  val Outside: State = -1

  val size = 7

  def newBoard(): Board = {
    val board = Array.ofDim[State](size, size)
    for (i <- outOfRanges) board(i._1)(i._2) = Outside
    board(3)(3) = Filled
    board
  }

  lazy val outOfRanges: List[(Int, Int)] = {
    val indexes = List(0, 1, 5, 6)
    val tuples = for (i <- indexes; j <- indexes) yield (i, j)
    tuples
  }

  def solve(): List[Board] = {
    def doSolve(frontier: List[List[Board]]): List[Board] = {
      val path = frontier.head
      if (path.nonEmpty) {
        val lastBoard = path.head
        for (i <- 0 to size; j <- 0 to size) {

        }
      }
      ???
    }

    doSolve(List(List(newBoard())))
  }


}

object PegSolitaireApp extends App with PegSolitaireGame {
  val board = newBoard()
}





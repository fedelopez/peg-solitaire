package cat.pseudocodi

trait PegSolitaireGame {

  val BoardSize = 7

  abstract case class Cell(x: Int)

  object Empty extends Cell(0)

  object Filled extends Cell(1)

  object Outside extends Cell(-1)

  case class Board(cells: Array[Array[Cell]]) {
    def isComplete: Boolean = {
      def pegCount(row: Int): Int = {
        val count = (0 until BoardSize).count(col => cells(row)(col) == Filled)
        count
      }

      val filledCount = (0 until BoardSize).foldLeft(0)((acc, row) => acc + pegCount(row))
      filledCount == 1
    }
  }

  case class Point(x: Int, y: Int)

  case class Move(from: Point, to: Point)

  def newBoard(): Board = {
    val board = Board(Array.fill(BoardSize, BoardSize)(Filled))
    for (i <- outOfRanges) board.cells(i.x)(i.y) = Outside
    board.cells(3)(3) = Empty
    board
  }

  lazy val outOfRanges: List[Point] = {
    val indexes = List(0, 1, 5, 6)
    for (i <- indexes; j <- indexes) yield Point(i, j)
  }

  def solve(): List[Move] = {
    val queue: collection.mutable.Queue[(Board, List[Move])] = collection.mutable.Queue((newBoard(), List()))
    val visited: List[Board] = List()
    while (queue.nonEmpty) {
      val element = queue.dequeue()
      if (element._1.isComplete) element._2
      else {

      }
    }

    List()
  }

  def states(board: Board): List[(Board, Move)] = {
    List()
  }

}

object PegSolitaireApp extends App with PegSolitaireGame {
  val board = newBoard()
  println(board)
}





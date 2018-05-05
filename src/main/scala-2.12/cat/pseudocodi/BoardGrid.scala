package cat.pseudocodi

import scala.collection.mutable

object BoardGrid {

  sealed abstract class Hole

  object Excluded extends Hole

  object Empty extends Hole

  object Peg extends Hole

  type Board = Array[Array[Hole]]
  val length: Int = 7

  def solution(): Seq[Board] = {
    val board: Board = initialise()
    var path: List[Board] = List(board)
    var visited: mutable.TreeSet[String] = mutable.TreeSet()
    var totalPegs = board.flatten.count(hole => hole == Peg)

    while (path.nonEmpty) {
      val head: Board = path.head
      visited += encode(head)

      val pegCount = head.flatten.count(hole => hole == Peg)
      if (pegCount == 1) {
        return path
      }
      if (pegCount < totalPegs) {
        totalPegs = pegCount
        println(pegCount)
      }

      val boards = expand(head)
      val unvisitedBoards: List[Board] = boards.filterNot(b => visited.contains(encode(b)))
      if (unvisitedBoards.nonEmpty) {
        path = unvisitedBoards.head :: path
      } else {
        path = path.tail
      }
    }
    List()
  }

  def initialise(): Board = {
    val board: Board = Array.fill[Hole](length, length)(Peg)
    board(3)(3) = Empty
    val excluded = List(
      (0, 0), (1, 0), (5, 0), (6, 0),
      (0, 1), (1, 1), (5, 1), (6, 1),
      (0, 5), (1, 5), (5, 5), (6, 5),
      (0, 6), (1, 6), (5, 6), (6, 6)
    )
    excluded.foreach(tuple => board(tuple._1)(tuple._2) = Excluded)
    board
  }

  def inBoard(x: Int, y: Int): Boolean = x > -1 & y > -1 && x < length && y < length

  def hasNorthMove(x: Int, y: Int, board: Board): Boolean =
    inBoard(x, y - 1) && board(x)(y - 1) == Peg && inBoard(x, y - 2) && board(x)(y - 2) == Peg

  def hasWestMove(x: Int, y: Int, board: Board): Boolean =
    inBoard(x - 1, y) && board(x - 1)(y) == Peg && inBoard(x - 2, y) && board(x - 2)(y) == Peg

  def hasEastMove(x: Int, y: Int, board: Board): Boolean =
    inBoard(x + 1, y) && board(x + 1)(y) == Peg && inBoard(x + 2, y) && board(x + 2)(y) == Peg

  def hasSouthMove(x: Int, y: Int, board: Board): Boolean =
    inBoard(x, y + 1) && board(x)(y + 1) == Peg && inBoard(x, y + 2) && board(x)(y + 2) == Peg

  def expand(board: Board): List[Board] = {
    var result = List[Board]()
    for (x <- board.indices; y <- board.indices if board(x)(y) == Empty) {
      if (hasNorthMove(x, y, board)) {
        val newBoard: Board = Array.tabulate(length, length)((i, j) => board(i)(j))
        newBoard(x)(y) = Peg
        newBoard(x)(y - 1) = Empty
        newBoard(x)(y - 2) = Empty
        result = newBoard :: result
      }
      if (hasWestMove(x, y, board)) {
        val newBoard: Board = Array.tabulate(length, length)((i, j) => board(i)(j))
        newBoard(x)(y) = Peg
        newBoard(x - 1)(y) = Empty
        newBoard(x - 2)(y) = Empty
        result = newBoard :: result
      }
      if (hasEastMove(x, y, board)) {
        val newBoard: Board = Array.tabulate(length, length)((i, j) => board(i)(j))
        newBoard(x)(y) = Peg
        newBoard(x + 1)(y) = Empty
        newBoard(x + 2)(y) = Empty
        result = newBoard :: result
      }
      if (hasSouthMove(x, y, board)) {
        val newBoard: Board = Array.tabulate(length, length)((i, j) => board(i)(j))
        newBoard(x)(y) = Peg
        newBoard(x)(y + 1) = Empty
        newBoard(x)(y + 2) = Empty
        result = newBoard :: result
      }
    }
    result
  }

  def encode(board: Board): String = {
    board.flatten.filterNot(e => e == Excluded).flatMap {
      case Empty => "E"
      case Peg => "P"
      case _ => ""
    }.mkString
  }
}

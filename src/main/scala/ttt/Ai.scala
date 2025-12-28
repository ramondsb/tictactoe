package ttt

import ttt.TicTacToe.Board.{isFull, matchSomeWinCondition}
import ttt.TicTacToe.{Board, CellType, Empty, Human, Machine, Move, O, Player, X}

import scala.util.Random

trait Ai {
  def nextMove(board: Board, cellType: CellType): Either[String, Move]
}

class RandomAi(random: Random, cellType: CellType) extends Ai {
  def nextMove(board: Board, cellType: CellType): Either[String, Move] = {
    ai(board, cellType)(random)
  }

  private def ai(board: Board, cellType: CellType)(implicit rand: Random): Either[String, Move] = {
    if (board.exists(_ == Empty)) {
      var address = -1
      do {
        address = rand.nextInt(9)
      } while (board(address) != Empty)
      Right(Move(address, cellType))
    } else Left("No more empty cells available")
  }
}


class Minmax extends Ai {
  override def nextMove(board: Board, cellType: CellType): Either[String, Move] = {
    val candidates = genCandidates(board, cellType)
    val bestCandidate = candidates.maxBy({
      case (_, position) => minmax(position, 1, false, cellType)
    })
    Right(bestCandidate._1)
  }

  private def minmax(board: Board, depth: Int, isMax: Boolean, cellType: CellType): Int = {
    if (depth <= 0) {
      return evaluate(board, cellType)
    }
    // Invert cellType
    val invertedCellType = if (cellType == X) O else X
    val candidates = genCandidates(board, invertedCellType)
    if (isMax) {
      candidates.map({ case (_, position) => minmax(position, depth - 1, !isMax, invertedCellType)}).max
    } else {
      candidates.map({ case (_, position) => minmax(position, depth - 1, !isMax, invertedCellType)}).min
    }
  }

  private def genCandidates(board: Board, cellType: CellType): Seq[(Move, Board)] = {
    board
      .zipWithIndex
      .filter(_._1 == Empty)
      .map(i => {
        val newPosition = board.clone()
        newPosition(i._2) = cellType
        val move = Move(i._2, cellType)
        (move, newPosition)
      })
      .toSeq
  }

  private def evaluate(board: TicTacToe.Board, cellType: CellType): Int = {
    // - Lost
    // 0 Draw
    // 1 Win
    val humanCellType = if (cellType == X) O else X
    (Board.matchSomeWinCondition(board, cellType), Board.matchSomeWinCondition(board, humanCellType)) match {
      case (true, false) => 1
      case (false, true) => -1
      case _ => 0
    }
  }
}
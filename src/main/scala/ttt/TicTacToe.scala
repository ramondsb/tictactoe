package ttt

import ttt.TicTacToe.Board.validateAddress

import scala.io.StdIn.readInt
import scala.util.Random

object TicTacToe {
  sealed trait Player {
    def cellType: CellType
    def nextMove(board: Board): Either[String, Move]
  }
  final case class Human(cellType: CellType) extends Player {
    def nextMove(board: Board): Either[String, Move] = {
      val address = readInt()
      if (validateAddress(board, address)) {
        Right(Move(address, cellType))
      } else Left("Invalid human address")
    }
  }
  final case class Machine(cellType: CellType, ai: Ai) extends Player {
    def nextMove(board: Board): Either[String, Move] = {
      ai.nextMove(board, cellType)
    }
  }

  sealed trait GameState
  final case object NotStarted extends GameState
  final case object OnGoing extends GameState
  final case class Finished(winner: Option[Player] = None) extends GameState

  sealed trait CellType
  final case object X extends CellType {
    override def toString = "X"
  }
  final case object O extends CellType {
    override def toString = "O"
  }
  final case object Empty extends CellType {
    override def toString = "_"
  }

  case class Move(address: Int, cellType: CellType)

  type Board = Array[CellType]

  object Board {

    def printBoard(board: Board): Unit = {
      for (i <- 0 until 9) {
        if (i % 3 == 0) println()
        print(board(i))
      }
      println()
    }

    def applyMove(board: Board, move: Move): Either[String, Board] = {
      if (board(move.address) == Empty)  {
        board(move.address) = move.cellType
        Right(board)
      } else Left("Non empty cell")
    }

    def validateAddress(board: Board, address: Int): Boolean = {
      if (!Range.inclusive(0, 8).contains(address))  return false
      if (board(address) != Empty) return false
      true
    }

    def matchSomeWinCondition(board: Board, cellType: CellType): Boolean =
      Seq(
        Seq(0, 1, 2), // First row from top to bottom
        Seq(3, 4, 5), // Second row
        Seq(6, 7, 8), // Third row
        Seq(0, 3, 6), // First column from left to right
        Seq(1, 4, 7), // Second column
        Seq(2, 5, 8), // column vertical
        Seq(0, 4, 8), // Main diagonal
        Seq(6, 4, 2), // Second diagonal
      ).exists(_.forall(i => board(i) == cellType))

    def isFull(board: Board): Boolean = if (board.count(_ != Empty) >= 9) true else false
  }
}

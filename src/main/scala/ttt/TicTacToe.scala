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
  final case class Machine(cellType: CellType, random: Random) extends Player {
    def nextMove(board: Board): Either[String, Move] = {
      ai(board, cellType)(random)
    }
    private def ai(board: Board, cellType: CellType)(implicit rand: Random): Either[String, Move] = {
      if (board.exists(_ == Empty)) {
        var address = -1
        do {
          address = rand.nextInt(9)
        } while (board(address) != Empty)
        Right(Move(address , cellType))
      } else Left("No more empty cells available")
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
  }
}

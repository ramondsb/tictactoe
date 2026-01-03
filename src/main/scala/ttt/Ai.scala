package ttt

import ttt.TicTacToe.{Board, -, CellType, Empty, Human, Machine, Move, NoneMove, O, PlaceMove, Player, X}

import scala.util.Random

trait Ai {
  def nextMove(board: Board, cellType: CellType): Either[String, Move]
}

class RandomAi(random: Random, cellType: CellType) extends Ai {
  def nextMove(board: Board, cellType: CellType): Either[String, PlaceMove] = {
    ai(board, cellType)(random)
  }

  private def ai(board: Board, cellType: CellType)(implicit rand: Random): Either[String, PlaceMove] = {
    if (board.exists(_ == Empty)) {
      var address = -1
      do {
        address = rand.nextInt(9)
      } while (board(address) != Empty)
      Right(PlaceMove(address, cellType))
    } else Left("No more empty cells available")
  }
}

class Minmax extends Ai {
  override def nextMove(board: Board, cellType: CellType): Either[String, Move] = {
    println("Minmax thinking...")
    Right(minmax(board, 9, false)._2)
  }
  def evaluatePosition(board: TicTacToe.Board): Int = {
    (Board.matchSomeWinCondition(board, X), Board.matchSomeWinCondition(board, O)) match {
      case (true, false) => 1
      case (false, true) => -1
      case _ => 0
    }
  }
  def isOver(position: Board): Boolean = !position.exists(_ == Empty)
  def nextPossiblePositions(board: TicTacToe.Board, cellType: CellType): Seq[(Board, Move)] = {
    // Find an empty cell and place a mark
    var positions = Seq.empty[(Board, Move)]
    for (i <- 0 until 9) {
      if (board(i) == Empty) {
        val newPosition = board.clone()
        newPosition(i) = cellType
        positions =  ((newPosition, PlaceMove(i, cellType)) +: positions)
      }
    }
    positions
  }

  def minmax(position: Board, depth: Int, isMaximizerTurn: Boolean): (Int, Move) = {
    if (depth == 0 || isOver(position)) {
      (evaluatePosition(position), NoneMove)
    } else {
      if (isMaximizerTurn) {
        val states = nextPossiblePositions(position, X).map {
          case (nextPostion, move) => (minmax(nextPostion, depth - 1, false), move)
        }
        val best = states.maxBy(_._1._1)
        (best._1._1, best._2)
      } else {
        val states = nextPossiblePositions(position, O).map {
          case (nextPosition, move) => (minmax(nextPosition, depth - 1, true), move)
        }
        val best = states.minBy(_._1._1)
        (best._1._1, best._2)
      }
    }
  }
}
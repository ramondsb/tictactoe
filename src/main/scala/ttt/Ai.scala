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

class Minmax2 extends Ai {
  /*
   Input: board, currentPlayer = true, depth = 2
   Output: best move for the current player
   */

  override def nextMove(board: Board, cellType: CellType): Either[String, Move] = {
    mi
  }
  // minmax(position, depth, player)
  // has a winner
  //     return 1 or -1
  // has no more where to play. It is a drawn
  //     return 0
  // if depth = 0
  //   return draw because it already passed throug the has winner section
  // list = generate plays possible for player and current position
  // states = for newPosition in list:
  //   minmax(newPositon, depth - 1, !player)
  // if player:
  //   min(states)
  // else:
  //   max(states)
  def minmax(board: Board, depth: Int, currentPlayer: CellType): (Int, Move) = {
    def hasWinner(board: TicTacToe.Board, ct: CellType): Option[CellType] = {
      if (matchSomeWinCondition(board, ct)) {
        Some(ct)
      } else None
    }

    def genCandidates(board: Board, cellType: CellType): Seq[(Move, Board)] = {
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

    val winner = hasWinner(board, currentPlayer)
    if (winner.isDefined) {
      winner.foreach(p => p match {
        case X => return 1
        case O => return -1
      })
    }

    if (isFull(board)) {
      return 0
    }

    if (depth == 0) {
      return 0
    }

    val newBoards = genCandidates(board, currentPlayer)

    currentPlayer match {
      case X => newBoards.map(x => (minmax(x._2, depth - 1, O))).minBy(x => x.)
      case O => newBoards.map(x => (minmax(x._2, depth - 1, X)).max
    }
  }

  // Read a board and player of turn
  // Check what is the game state
  //  if it is a final
  //    there is a winner or a draw? Return indication of that
  //  if it is not a final
  //    generate all possible moves for current player and get best state for current player
  //      getFinalState(newBoardState, nextPlayer)
  /*      for each new board state check what is the game state
            while not in a final state
              generate all possibles moves for the next player

  * */
  //m    call minmax again
}
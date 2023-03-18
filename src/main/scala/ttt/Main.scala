package ttt

import ttt.TicTacToe.Board.{applyMove, printBoard}
import ttt.TicTacToe.{Board, CellType, Empty, Finished, GameState, Human, Machine, Move, NotStarted, O, OnGoing, Player, X}

import scala.util.Random


object Main extends App {

  def printPreamble(): Unit = println("Hello, TicTacToe!")

  def checkGameState(board: Board, players: Seq[Player]): GameState = {
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

    val maybeWinner = players.filter(p => matchSomeWinCondition(board, p.cellType)).headOption

    maybeWinner
      .map(p => Finished(Some(p)))
      .getOrElse({
        if (board.count(_ != Empty) >= 9)  Finished() else  OnGoing
      })
  }

  implicit val randomGenerator = new Random()
  var gameState: GameState = NotStarted
  var human: Human = Human(X)
  var computer: Machine = Machine(O, randomGenerator)
  val players: Seq[Player] = Seq(human, computer)
  var board: Board = Array(Empty)
  var currentTurn: Player = human
  var isGameOver: Boolean = false

  printPreamble()

  do {
    gameState match {
      case NotStarted => {
        board = Array.fill(9)(Empty)
        gameState = OnGoing
      }
      case OnGoing => {
        val process = for {
          aMove <- currentTurn match {
            case Human(cell) => {
              currentTurn = computer
              println("Human turn...")
              human.nextMove(board)
            }
            case Machine(cell, _) => {
              println("Machine turn...")
              currentTurn = human
              computer.nextMove(board)
            }
          }
          updatedBoard <- applyMove(board, aMove)
        } yield {
          board = updatedBoard
          printBoard(updatedBoard)
          gameState = checkGameState(board, players)
        }
        process.left.foreach(err => println(s"Some error: ${err}"))
      }
      case Finished(winner) => {
        winner match {
          case Some(Human(_)) => println("Human won!")
          case Some(Machine(_, _)) => println("Machine won!")
          case None => println("Game draw!")
        }
        isGameOver = true
      }
    }
  } while (!isGameOver)
}

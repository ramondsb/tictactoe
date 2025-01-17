package ttt

import ttt.TicTacToe.Board.{applyMove, printBoard}
import ttt.TicTacToe.{Board, CellType, Empty, Finished, GameState, Human, Machine, Move, NotStarted, O, OnGoing, Player, X}

import scala.util.Random


object Main extends App {

  def printPreamble(): Unit = println("""
                                      |A tic-tac-toe game written in Scala!
                                      |Board is enumerated from 0 to 8:
                                      |0 1 2
                                      |3 4 5
                                      |6 7 8""".stripMargin)

  def checkGameState(board: Board, players: Seq[Player]): GameState = {
    // TODO: Replace with find method
    val maybeWinner = players
      .filter(p => Board.matchSomeWinCondition(board, p.cellType))
      .headOption
    maybeWinner
      .map(p => Finished(Some(p)))
      .getOrElse({ if (Board.isFull(board)) Finished() else  OnGoing})
  }

  implicit val randomGenerator = new Random()
  var gameState: GameState = NotStarted
  var human: Human = Human(X)
  //var computer: Machine = Machine(O, new RandomAi(randomGenerator,O))
  var computer: Machine = Machine(O, new Minmax2())
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
              println("Human turn...")
              human.nextMove(board)
            }
            case Machine(cell, _) => {
              println("Machine turn...")
              computer.nextMove(board)
            }
          }
          _ <- Right({ currentTurn = if (currentTurn == computer) human else computer })
          updatedBoard <- applyMove(board, aMove)
          _ <- Right({
            board = updatedBoard
            printBoard(updatedBoard)
            gameState = checkGameState(board, players)
          })
        } yield ()
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

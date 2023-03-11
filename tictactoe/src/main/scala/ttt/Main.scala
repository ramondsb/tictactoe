package ttt

import scala.io.StdIn.readInt
import scala.util.Random


object Main extends App {
  sealed trait Player
  final case class Human(cellType: CellType) extends Player
  final case class Machine(cellType: CellType) extends Player

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

  implicit val randomGenerator = new Random()

  def printBoard(board: Board): Unit = {
    for (i <- 0 until 9) {
      if (i % 3 == 0) println()
      print(board(i))
    }
    println()
  }

  // TODO: Check immutable structure to hold board
  def applyMove(board: Board, move: Move): Either[String, Board] = {
    board(move.address) = move.cellType
    Right(board)
  }

  def nextMove(board: Board, cellType: CellType)(implicit rand: Random): Either[String, Move] = {
    if (board.exists(_ == Empty)) {
      var address = -1
      do {
        address = rand.nextInt(9)
      } while (board(address) != Empty)
      Right(Move(address , cellType))
    } else Left("No more empty cells available")
  }

  def printPreamble(): Unit = println("Hello, TicTacToe!")

  def validateAddress(board: Main.Board, address: Int): Boolean = {
    // Range
    // Cell not occupied
     if (!Range.inclusive(0, 9).contains(address))  return false
    true
  }

  var gameState: GameState = NotStarted
  var human: Human = Human(X)
  var computer: Machine = Machine(O)
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
        printBoard(board)

        val aMove = currentTurn match {
          case Human(cell) => {
            currentTurn = computer
            println("Human turn...")
            val address = readInt()
            if (validateAddress(board, address)) {
              Right(Move(address, cell))
            } else Left("Invalid human address")
          }
          case Machine(cell) => {
            println("Machine turn...")
            currentTurn = human
            nextMove(board, cell)
          }
        }

        val result = aMove match {
          case Left(errorMsg) => Left(errorMsg)
          case Right(move) =>
            applyMove(board, move) match {
              case Left(errorMsg) => Left(errorMsg)
              case Right(updatedBoard) => {
                Right(updatedBoard)
              }
            }
        }

        result match {
          case Right(updatedBoard) => {
            board = updatedBoard
            // Check state for draw or victory
            gameState
            if (board.count(_ != Empty) >= 9) gameState = Finished() else  OnGoing
          }
          case Left(errorMsg) => println(errorMsg)
        }
      }
      case Finished(winner) => {
        winner match {
          case Some(Human(_)) => println("Human won!")
          case Some(Machine(_)) => println("Machine won!")
          case None => println("Game draw!")
        }
        isGameOver = true
      }
    }
  } while (!isGameOver)
}

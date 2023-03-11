package ttt

import scala.io.StdIn.readInt
import scala.util.Random


object Main extends App {
  sealed trait Player { def cellType: CellType }
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
    if (!Range.inclusive(0, 9).contains(address))  return false
    if (board(address) != Empty) return false
    true
  }

  def checkGameState(board: Main.Board, players: Seq[Player]): GameState = {
    def allMatchHorizontal(board: Board, cellType: CellType): Boolean =
      Seq(
        Seq(0, 1, 2), // First horizontal from top to bottom
        Seq(3, 4, 5), // Second vertical
        Seq(6, 7, 8)  // Third vertical
      ).exists(_.forall(i => board(i) == cellType))

    def allMatchVertical(board: Board, cellType: CellType): Boolean =
      Seq(
        Seq(0, 3, 6), // First vertical from left to right
        Seq(1, 4, 7), // Second vertical
        Seq(2, 5, 7)  // Third vertical
      ).exists(_.forall(i => board(i) == cellType))

    def allDiagonals(board: Board, cellType: CellType): Boolean =
      Seq(
        Seq(0, 4, 8), // Main diagonal
        Seq(6, 4, 2), // Second diagonal
      ).exists(_.forall(i => board(i) == cellType))

    val hasWinner = {
      players.filter(p =>
        (allMatchHorizontal(board, p.cellType) ||
          allMatchVertical(board, p.cellType) ||
          allDiagonals(board, p.cellType))
      ).headOption
    }

    hasWinner
      .map(p => Finished(Some(p)))
      .getOrElse({
        if (board.count(_ != Empty) >= 9)  Finished() else  OnGoing
      })
  }

  var gameState: GameState = NotStarted
  var human: Human = Human(X)
  var computer: Machine = Machine(O)
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
        for {
          aMove <- currentTurn match {
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
          updatedBoard <- applyMove(board, aMove)
        } yield {
          board = updatedBoard
          printBoard(board)
          gameState = checkGameState(board, players)
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

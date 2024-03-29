package ttt

import ttt.Main.{checkGameState, randomGenerator}
import ttt.TicTacToe.{Board, CellType, Empty, Finished, Human, Machine, O, X}

class MainSpec extends munit.FunSuite {
    val ai = new RandomAi(randomGenerator, O)
    val players = Seq(Human(X), Machine(O, ai))
    test("row victory") {
      val board: Board = Array[CellType](
        X, X, X,
        O, X, O,
        Empty, Empty, Empty
      )
      val obtained = checkGameState(board, players)
      val expected = Finished(Some(Human(X)))
      assertEquals(obtained, expected)
    }

    test("column victory") {
      val board: Board = Array[CellType](
        O, X, X,
        O, X, O,
        O, Empty, Empty
      )
      val obtained = checkGameState(board, players)
      val expected = Finished(Some(Machine(O, ai)))
      assertEquals(obtained, expected)
    }

    test("Draw") {
      val board: Board = Array[CellType](
        O, X, O,
        X, O, X,
        X, O, X
      )
      val obtained = checkGameState(board, players)
      val expected = Finished(None)
      assertEquals(obtained, expected)
    }

  // TODO: Check error msg when human inputs a invalid address
}
package ttt

import ttt.Main.{checkGameState, randomGenerator}
import ttt.TicTacToe.{-, Board, CellType, E, Finished, Human, Machine, NoneMove, O, PlaceMove, X}

class MainSpec extends munit.FunSuite {
    val ai = new RandomAi(randomGenerator, O)
    val players = Seq(Human(X), Machine(O, ai))
    test("row victory") {
      val board: Board = Array[CellType](
        X, X, X,
        O, X, O,
        E, E, E
      )
      val obtained = checkGameState(board, players)
      val expected = Finished(Some(Human(X)))
      assertEquals(obtained, expected)
    }

    test("column victory") {
      val board: Board = Array[CellType](
        O, X, X,
        O, X, O,
        O, E, E
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
  test("Ai2") {
    val ai = new Minmax()
    val board: Board = Array[CellType](
      O, X, E,
      X, O, X,
      X, O, X
    )
    val ans = ai.minmax(board, 3, false)
    assertEquals(ans._2, TicTacToe.PlaceMove(2, O))
  }

  test("Draw2") {
    val ai = new Minmax()
    val board: Board = Array[CellType](
      O, X, O,
      X, O, X,
      X, O, X
    )
    val ans = ai.minmax(board, 3, true)
    assertEquals(ans._2, TicTacToe.NoneMove)
  }
  test("4") {
    val ai = new Minmax()
    val board: Board = Array[CellType](
      X, E, E,
      E, X, E,
      E, E, E
    )
    val ans = ai.minmax(board, 9, false)
    assertEquals(ans._2, TicTacToe.PlaceMove(8, O))
  }
  test("5") {
    val ai = new Minmax()
    val board: Board = Array[CellType](
      X, E, E,
      E, X, E,
      E, O, O
    )
    val ans = ai.minmax(board, 9, false)
    assertEquals(ans._2, TicTacToe.PlaceMove(6, O))
  }
  test("6") {
    val ai = new Minmax()
    val board: Board = Array[CellType](
      X, O, X,
      E, X, E,
      X, E, O
    )
    val ans = ai.minmax(board, 9, false)
    assertEquals(ans._2, TicTacToe.PlaceMove(3, O))
  }
}
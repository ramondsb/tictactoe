package ttt

import ttt.Main.{Board, CellType, Empty, Finished, Human, Machine, O, X, checkGameState, randomGenerator}

class MainSpec extends munit.FunSuite {
    val players = Seq(Human(X), Machine(O, randomGenerator))
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
      val expected = Finished(Some(Machine(O, randomGenerator)))
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
}
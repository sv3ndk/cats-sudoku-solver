package svend.sudoku

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*
import cats.effect.unsafe.IORuntime

class FS2SolverTest extends AnyFlatSpec with must.Matchers {

  // probably not the best way to unit test an IO, though that's what I know for now...
  import cats.effect.unsafe.implicits.global

  behavior of "fs2-solver"

  it must "provide a valid solution to the easy problem" in {
    val game = FS2Solver.solve(Game.easy).unsafeRunSync()
    game.isFinished mustBe (true)
    game.isSolvedCorrectly mustBe (true)
  }
}

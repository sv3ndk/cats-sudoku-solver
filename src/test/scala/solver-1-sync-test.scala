package svend.sudoku

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*

class SyncSolverTest extends AnyFlatSpec with must.Matchers {

  behavior of "sync-solver"

  it must "provide a valid solution to the easy problem" in {
    SyncSolver.solve(Game.easy).isFinished mustBe (true)
    SyncSolver.solve(Game.easy).isSolvedCorrectly mustBe (true)
  }
}

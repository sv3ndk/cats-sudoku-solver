package svend.sudoku

import scala.annotation.tailrec

// Naive synchronous solver implementation, simply looping through all
// tiles over and over until they're all solved (or forever...)
// There is no back-tracking here, the algo only moves forward when there
// is one clear solution for a tile

object SyncSolver {

  def solve(game: Game): Game = {

    @tailrec
    def pass(currentGame: Game): Game =
      if (currentGame.isFinished) currentGame
      else {
        val processedGame = currentGame.pendingTiles.foldLeft(currentGame) { (gam, pending) =>
          val excludedValues = gam
            .peersOf(pending.coord)
            .flatMap { peer =>
              peer match {
                case Pending(_, candidates) => None
                case Solved(_, value)       => Some(value)
              }
            }
            .toSet

          gam.replaceTile(pending.excludeCandidates(excludedValues))
        }
        // we should probably check that we're making progress here...
        pass(processedGame)
      }

    pass(game)
  }
}

object SyncSolverDemo {
  def main(args: Array[String]): Unit = {
    println("Sync sudoku solver")
    println(s"Using basic sync solver on \n${Game.easy.printableString}...")
    val solution = SyncSolver.solve(Game.easy)
    println(s"tada: \n${solution.printableString}")
  }
}

package svend.sudoku

import scala.annotation.tailrec

// Naive synchronous solver implementation, simply looping through all
// tiles over and over until they're all solved.
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
            .peers(pending.coord)
            .flatMap { tile =>
              tile.value match {
                case TileValue.Pending(candidates) => None
                case TileValue.Solution(value)     => Some(value)
              }
            }
            .toSet

          gam.replaceTile(pending.excludeValues(excludedValues))
        }
        pass(processedGame)
      }

    pass(game)
  }
}

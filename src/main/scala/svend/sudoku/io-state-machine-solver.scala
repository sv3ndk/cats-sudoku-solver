package svend.sudoku
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Deferred

// my own solution for a cats IO-based implementation
object IOStateSolver {

  trait Solver {
    def remove(value: Int): IO[Unit]
    def solveWith(value: Int): IO[Unit]
  }

  object Solver {
    def create(coord: Coord): IO[Solver] = {

      for {
        deferredResult <- IO.deferred[Tile]
        state <- IO.ref(Tile.unknownTile(coord))
      } yield new Solver {

        def remove(value: Int): IO[Unit] =
          state
            .updateAndGet(currentTile => currentTile.excludeCandidate(value))
            .flatMap(tile =>
              if (tile.isFinished)
                deferredResult.complete(tile).void
                // todo: we also have to remove our value from all peers here
              else
                IO.unit
            )

        def solveWith(value: Int): IO[Unit] =
          state
            .updateAndGet(tile => Tile.known(tile.coord.row, tile.coord.col, value))
            .flatMap(tile => deferredResult.complete(tile))
            .void
      }

    }
  }

  // blocks on the solution of a solver, and dispatch it to the peer tiles when availble
  // def dispatch(sourceSolution: Deferred[IO, Tile], peerSolvers: List[Solver]): IO[Deferred[IO, Tile]] = for {
  //   result <- IO.deferred[Tile]
  //   tile <- sourceSolution.get
  //   _ <- peerSolvers.traverse(_.remove(result.v))
  //   // ..
  //   _ <- result.complete(tile)
  // } yield result

  def solve(game: Game): IO[Game] = {
// from a Game instance:
    // build 81 solvers
    // block on the recepion of the 81 deferred results
    // solveWith() all the given values
    // => if the algo is successful, the 81 results should arrive
    // how does a solved tile can access its peers from here?''
    ???
  }
}

object IOStateSolverDemo extends IOApp.Simple {

  def run: IO[Unit] = IOStateSolver
    .solve(Game.easy)
    .flatMap(IO.println)
    .void
}

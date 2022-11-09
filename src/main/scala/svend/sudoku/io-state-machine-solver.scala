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
        deferredResult <- IO.deferred[Solved]
        state <- IO.ref(Tile.unknown(coord))
      } yield new Solver {

        def remove(value: Int): IO[Unit] =
          state
            .updateAndGet(currentTile => currentTile.excludeCandidate(value))
            .flatMap {
              case solved @ Solved(_, _) =>
                // if (tile.isFinished)
                deferredResult.complete(solved).void
              // todo: we also have to remove our value from all peers here
              case _ =>
                IO.unit
            }

        def solveWith(value: Int): IO[Unit] =
          state
            .modify(tile =>
              val solved = Solved(tile.coord, value)
              (solved, solved)
            )
            .flatMap(solved => deferredResult.complete(solved))
            .void
      }

    }
  }

  // blocks on the solution of a solver, and dispatch it to the peer tiles when availble
  def dispatch(sourceSolution: Deferred[IO, Solved], peerSolvers: List[Solver]): IO[Deferred[IO, Solved]] = for {
    result <- IO.deferred[Solved]
    tile <- sourceSolution.get
    // _ <- peerSolvers.traverse(_.remove(result.v))
    // ..
    _ <- result.complete(tile)
  } yield result

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

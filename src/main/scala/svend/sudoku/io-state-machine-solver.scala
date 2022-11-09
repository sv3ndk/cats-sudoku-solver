package svend.sudoku
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Deferred

// my own solution for a cats IO-based implementation
object IOStateSolver {

  trait Solver {
    // message received from a peer that a candidate is no longer available
    def remove(candidate: Int): IO[Unit]

    // forces an explicit solution (during the initialization of the game)
    def solveWith(solution: Int): IO[Unit]

    def solution: Deferred[IO, Solved]
  }

  object Solver {
    def create(coord: Coord): IO[Solver] =
      for {
        deferredResult <- IO.deferred[Solved]
        state <- IO.ref(Tile.unknown(coord))
      } yield new Solver {

        def remove(candidate: Int): IO[Unit] = {
          state
            .updateAndGet(_.excludeCandidate(candidate))
            .flatMap {
              case solved @ Solved(_, _) => deferredResult.complete(solved).void
              case _                     => IO.unit
            }
        }

        def solveWith(solution: Int): IO[Unit] = {
          state
            .modify(tile =>
              val solved = Solved(tile.coord, solution)
              (solved, solved)
            )
            .flatMap(solved => deferredResult.complete(solved))
            .void
        }

        def solution = deferredResult
      }
  }

  import cats.syntax.traverse.*
  import cats.syntax.parallel.*

  // blocks on the solution of a solver and dispatches it to the peer solveds when availble
  def proxyDispatch(sourceSolution: Deferred[IO, Solved], peerSolvers: List[Solver]): IO[Deferred[IO, Solved]] = for {
    result <- IO.deferred[Solved]
    solved <- sourceSolution.get
    _ <- peerSolvers.parTraverse(_.remove(solved.solution))
    _ <- result.complete(solved)
  } yield result

  def solve(game: Game): IO[Game] = {
    // building 1 solver per tile
    val solversIos: IO[Map[Coord, Solver]] = game.tiles
      .parTraverse(tile => Solver.create(tile.coord).map(solver => (tile.coord -> solver)))
      .map(_.toMap)

    for {
      solvers <- solversIos

      // setting the initial value of the given tiles
      _ <- game.solvedTiles.parTraverse { case Solved(coord, solution) => solvers(coord).solveWith(solution) }

      // plugin the solution dispatchers
      solutions <- solvers.toList.parTraverse { (coord, solver) =>
        proxyDispatch(
          solver.solution,
          game.peers(coord).map(peerTile => solvers(peerTile.coord))
        )
      }

      // let it brew..
      solvedTiles <- solutions.traverse(_.get)
    } yield Game.create(solvedTiles)

  }
}

object IOStateSolverDemo extends IOApp.Simple {

  def run: IO[Unit] = for {
    problem <- IO.pure(Game.easy)
    _ <- IO.println(s"solving \n${problem.printableString}")
    solution <- IOStateSolver.solve(problem)
    _ <- IO.println(s"tada \n${solution.printableString}")
  } yield ()
}

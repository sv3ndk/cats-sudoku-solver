package svend.sudoku

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Deferred

/** My own solution for a cats IO-based implementation.
  *
  * The entry point builds one solver for each tile, starts them all in parallel and lets each solved one notify its
  * peers that one candidate value should be removed. At some point, enough `.remove()` invocations have been received
  * s.t only one candidate remains, in which case the tile becomes "solved" and again broadcasts its value to its peers.
  *
  * Those .remove() kinda make this a "push-based" approach. The blog post below describes a "poll-based" one, where
  * each tile is launching a bunch of blocking .getValue() on all its peers in parallel, and when enough have been
  * succesful, updates its state, which then becomes available to all its peers pulling on it.
  * https://medium.com/@fqaiser94/concurrent-sudoku-solver-part-2-using-cats-effect-ref-deferred-io-race-a380a182c233
  *
  * The part 3 of the blog series is actually a "push-based" approach, using cats queues:
  * https://medium.com/@fqaiser94/concurrent-sudoku-solver-part-1-single-candidate-technique-domain-modelling-6c885a1e4ef3f
  *
  * This solver can only solve easy problems, i.e. only those that never need to branch between various branches.
  */
object IOStateSolver {

  trait TileSolver {
    def coord: Coord
    def remove(candidate: Int): IO[Unit]
    def solution: IO[Solved]
  }

  object TileSolver {

    def create(tile: Tile): IO[TileSolver] = tile match {
      case solved @ Solved(_, _)   => createSolved(solved)
      case pending @ Pending(_, _) => createPending(pending)
    }

    // fake solver for a tile that's already solved from the start
    private def createSolved(tile: Solved): IO[TileSolver] = IO.pure(new TileSolver {
      export tile.coord
      def remove(candidate: Int): IO[Unit] = IO.unit
      def solution: IO[Solved] = IO.pure(tile)
    })

    private def createPending(tile: Pending): IO[TileSolver] =
      for {
        deferredSolution <- IO.deferred[Solved]
        tileState <- IO.ref[Tile](tile)
      } yield new TileSolver {
        def coord: Coord = tile.coord

        def remove(candidate: Int): IO[Unit] =
          tileState
            .updateAndGet(_.excludeCandidate(candidate))
            .flatMap {
              case solved @ Solved(_, _) => deferredSolution.complete(solved).void
              case _                     => IO.unit
            }

        // not providing the Deferred instance directly achieves a better encapsulation since
        // we are then the only componenent that can .complete() it
        def solution = deferredSolution.get
      }
  }

  import cats.syntax.traverse.*
  import cats.syntax.parallel.*

  // blocks until that solver solution is available, then dispatches it the solvers of the peer tiles
  def proxyDispatch(upstreamSolution: IO[Solved], peerSolvers: List[TileSolver]): IO[Deferred[IO, Solved]] = for {
    result <- IO.deferred[Solved]
    solved <- upstreamSolution
    _ <- peerSolvers.parTraverse(_.remove(solved.solution))
    _ <- result.complete(solved)
  } yield result

  def solve(game: Game): IO[Game] =
    for {
      solvers <- game.tiles.parTraverse(TileSolver.create)
      proxiedSolutions <- solvers.parTraverse { solver =>
        proxyDispatch(
          solver.solution,
          game
            .peers(solver.coord)
            .flatMap(peerTile => solvers.find(_.coord == peerTile.coord))
        )
      }
      solvedTiles <- proxiedSolutions.traverse(_.get)
    } yield Game.create(solvedTiles)

}

object IOStateSolverDemo extends IOApp.Simple {

  def run: IO[Unit] = for {
    problem <- IO.pure(Game.easy)
    _ <- IO.println(s"solving \n${problem.printableString}")
    solution <- IOStateSolver.solve(problem)
    _ <- IO.println(s"tada \n${solution.printableString}")
  } yield ()
}

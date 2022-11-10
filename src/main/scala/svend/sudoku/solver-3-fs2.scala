package svend.sudoku
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Resource
import fs2.concurrent.Topic
import fs2.Stream
import cats.syntax.traverse.given

/** FS2-based Sudoko solver.
  *
  * This is essentially a re-implementation of the algorithm presented in the blog, the only different is the way in
  * which I push the known tile values to the central topic
  * https://medium.com/@fqaiser94/concurrent-sudoku-solver-part-4-using-fs2-stream-topic-949c8b099abb
  *
  * logic:
  *   - one stream per tile, each providing one single solution then closing
  *   - one central topic where all tile solution the solution of each tile stream is published
  *   - there are 2 kinds of tile streams:
  *     - tiles that are already solved simply push their result directly
  *     - pending tiles listen to the topic until solution for all their peers is received, then deduce their solution,
  *       publish it and terminate
  *   - all stream tiles are gathered into one which is compiled into a List of solved times, which is the solution
  *
  * Note: this approach is easy to get wrong: I failed a couple of times with an algorithm that was not starting either
  * because the known tiles were published before the other tiles started listening, or because the flow was never
  * reaching the point where I was publishing them.
  *
  * This solution is also slower, I guess FS2 Streams are heavier beasts than simple IOs
  */

object FS2Solver {

  def solverStream(tile: Tile, solutionTopic: Topic[IO, Solved]): Resource[IO, Stream[IO, Solved]] =
    tile match {
      case solved @ Solved(_, _)   => solvedStream(solved, solutionTopic)
      case pending @ Pending(_, _) => pendingStream(pending, solutionTopic)
    }

  /** solver stream for an already solved pile: just pushing the known result
    */
  def solvedStream(solved: Solved, solutionTopic: Topic[IO, Solved]): Resource[IO, Stream[IO, Solved]] =
    Resource.pure(Stream(solved))

  /** solver stream for a pending tile: listening to all peers via the topic, then become Solved and finishes
    */
  def pendingStream(pending: Pending, solutionTopic: Topic[IO, Solved]): Resource[IO, Stream[IO, Solved]] =
    solutionTopic
      .subscribeAwait(81)
      .map(solvedStream =>
        solvedStream
          .filter(solved => solved.coord.isPeerOf(pending.coord))
          .mapAccumulate[Tile, Tile](pending) { (currentTile, solved) =>
            val updated = currentTile.excludeCandidate(solved.solution)
            (updated, updated)
          }
          .collectFirst { case (_, solved @ Solved(_, _)) => solved }
      )

  /** entry-point: wiring it all together
    */
  def solve(game: Game): IO[Game] = {
    Topic[IO, Solved]
      .flatMap { solutionTopic =>
        game.tiles
          .traverse(solverStream(_, solutionTopic))
          .use { (solvedStream: List[Stream[IO, Solved]]) =>
            solvedStream
              .reduce(_ merge _)
              .evalTap(solved => solutionTopic.publish1(solved))
              .compile
              .toList
              .map(Game.create)
          }
      }
  }
}

object FS2SolverDemo extends IOApp.Simple {
  def run: IO[Unit] = for {
    problem <- IO.pure(Game.easy)
    _ <- IO.println(s"Using FS2 solver on \n${problem.printableString}")
    solution <- FS2Solver.solve(problem)
    _ <- IO.println(s"tada \n${solution.printableString}")
  } yield ()

}

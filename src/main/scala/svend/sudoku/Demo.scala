package svend.sudoku

import cats.effect.IO

object HelloWorld {

  @main def run() = {
    println("Sudoko solver")

    println(s"solving \n${easy.printableString}...")
    val solution = SyncSolver.solve(easy)
    println(s"tada: \n${solution.printableString}")

  }

  // problem that does not necessitate back-tracking, you can always deduce the next step without ambiguity
  val easy = Game.create(
    List(
      Tile(0, 1, 6),
      Tile(0, 3, 3),
      Tile(0, 6, 8),
      Tile(0, 8, 4),
      Tile(1, 0, 5),
      Tile(1, 1, 3),
      Tile(1, 2, 7),
      Tile(1, 4, 9),
      Tile(2, 1, 4),
      Tile(2, 5, 6),
      Tile(2, 6, 3),
      Tile(2, 8, 7),
      Tile(3, 1, 9),
      Tile(3, 4, 5),
      Tile(3, 5, 1),
      Tile(3, 6, 2),
      Tile(3, 7, 3),
      Tile(3, 8, 8),
      Tile(5, 0, 7),
      Tile(5, 1, 1),
      Tile(5, 2, 3),
      Tile(5, 3, 6),
      Tile(5, 4, 2),
      Tile(5, 7, 4),
      Tile(6, 0, 3),
      Tile(6, 2, 6),
      Tile(6, 3, 4),
      Tile(6, 7, 1),
      Tile(7, 4, 6),
      Tile(7, 6, 5),
      Tile(7, 7, 2),
      Tile(7, 8, 3),
      Tile(8, 0, 1),
      Tile(8, 2, 2),
      Tile(8, 5, 9),
      Tile(8, 7, 8)
    )
  )

}

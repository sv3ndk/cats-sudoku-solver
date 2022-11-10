package svend.sudoku

case class Coord(row: Int, col: Int) {
  def sameRow(other: Coord): Boolean = row == other.row
  def sameCol(other: Coord): Boolean = col == other.col
  def sameBox(other: Coord): Boolean =
    (row / 3 == other.row / 3) && (col / 3 == other.col / 3)

  def isPeerOf(other: Coord): Boolean = (sameBox(other) || sameRow(other) || sameCol(other)) && this != other
}

object Coord {
  def allCoords: List[Coord] = for {
    row <- (0 to 8).toList
    col <- (0 to 8)
  } yield Coord(row, col)
}

sealed trait Tile {
  val coord: Coord
  def excludeCandidates(excluded: Set[Int]): Tile = this match {
    case Solved(_, value) => this
    case Pending(coord, candidates) =>
      val remaining = candidates -- excluded
      if (remaining.size == 1) Solved(coord, remaining.head)
      else Pending(coord, candidates = remaining)
  }
  def excludeCandidate(excluded: Int): Tile = excludeCandidates(Set(excluded))

}

case class Solved(coord: Coord, solution: Int) extends Tile
case class Pending(coord: Coord, candidates: Set[Int]) extends Tile

object Tile {
  val validValues = (1 to 9).toSet
  def solved(row: Int, col: Int, solution: Int): Tile = Solved(Coord(row, col), solution)
  def unknown(coord: Coord): Tile = Pending(coord, validValues)
}

opaque type Game = List[Tile]

extension (game: Game) {

  def valueAt(coord: Coord): Tile = game.filter(_.coord == coord).head
  def valueAt(row: Int, col: Int): Tile = game.valueAt(Coord(row, col))
  def replaceTile(tile: Tile): Game = game.filter(_.coord != tile.coord) :+ tile
  def peers(of: Coord): List[Tile] = game.filter(_.coord.isPeerOf(of))

  def tiles: List[Tile] = game
  def pendingTiles: List[Pending] = tiles.flatMap {
    case p @ Pending(coord, candidates) => Some(p)
    case _                              => None
  }
  def solvedTiles: List[Solved] = tiles.flatMap {
    case s @ Solved(coord, solution) => Some(s)
    case _                           => None
  }
  def row(index: Int): List[Tile] = (0 to 8).map(valueAt(index, _)).toList
  def rows: List[List[Tile]] = (0 to 8).map(row).toList
  def column(index: Int): List[Tile] = (0 to 8).map(valueAt(_, index)).toList
  def columns: List[List[Tile]] = (0 to 8).map(column).toList
  def box(boxRow: Int, boxCol: Int): List[Tile] = for {
    row <- (boxRow * 3 to boxRow * 3 + 2).toList
    col <- (boxCol * 3 to boxCol * 3 + 2)
  } yield valueAt(row, col)
  def boxes: List[List[Tile]] = for {
    boxRow <- List(0, 1, 2)
    boxCol <- List(0, 1, 2)
  } yield box(boxRow, boxCol)

  def isFinished: Boolean = pendingTiles.isEmpty
  def isSolvedCorrectly: Boolean = {

    def validSolvedTiles(tiles: List[Tile]): Boolean = {
      val values = tiles.flatMap {
        case Solved(coord, solution) => Some(solution)
        case _                       => None
      }
      values.size == 9 && values.toSet == Tile.validValues
    }

    (rows ++ columns ++ boxes).forall(validSolvedTiles)
  }

  def printableString: String = {
    val border = "-" * (9 * 3 + 4) + "\n"
    val grid = for {
      row <- (0 to 8)
      col <- (0 to 8)

    } yield {
      val prefix = if (col % 3 == 0) "| " else " "
      val postfix = if (col == 8) " |\n" else " "
      val topLine = if (row == 0 && col == 0) border else ""
      val bottomLine = if (row % 3 == 2 && col == 8) border else ""

      val printedValue = game.valueAt(row, col) match {
        case Solved(_, value) => value.toString()
        case Pending(_, _)    => " "
      }

      topLine + prefix + printedValue + postfix + bottomLine
    }

    grid.mkString("")
  }

}

object Game {

  /** creates a game with 81 tiles with those initial values, or unknown when unspecified
    */
  def create(init: List[Tile]): Game =
    init.foldLeft(Game.empty) { (game, initTile) => game.replaceTile(initTile) }

  val empty: Game = Coord.allCoords.map(Tile.unknown)

  // -------------------------------
  // |    6    | 3       | 8     4 |
  // | 5  3  7 |    9    |         |
  // |    4    |       6 | 3     7 |
  // -------------------------------
  // |    9    |    5  1 | 2  3  8 |
  // |         |         |         |
  // | 7  1  3 | 6  2    |    4    |
  // -------------------------------
  // | 3     6 | 4       |    1    |
  // |         |    6    | 5  2  3 |
  // | 1     2 |       9 |    8    |
  // -------------------------------
  // problem that does not necessitate back-tracking, you can always deduce the next step without ambiguity
  val easy = Game.create(
    List(
      Tile.solved(0, 1, 6),
      Tile.solved(0, 3, 3),
      Tile.solved(0, 6, 8),
      Tile.solved(0, 8, 4),
      Tile.solved(1, 0, 5),
      Tile.solved(1, 1, 3),
      Tile.solved(1, 2, 7),
      Tile.solved(1, 4, 9),
      Tile.solved(2, 1, 4),
      Tile.solved(2, 5, 6),
      Tile.solved(2, 6, 3),
      Tile.solved(2, 8, 7),
      Tile.solved(3, 1, 9),
      Tile.solved(3, 4, 5),
      Tile.solved(3, 5, 1),
      Tile.solved(3, 6, 2),
      Tile.solved(3, 7, 3),
      Tile.solved(3, 8, 8),
      Tile.solved(5, 0, 7),
      Tile.solved(5, 1, 1),
      Tile.solved(5, 2, 3),
      Tile.solved(5, 3, 6),
      Tile.solved(5, 4, 2),
      Tile.solved(5, 7, 4),
      Tile.solved(6, 0, 3),
      Tile.solved(6, 2, 6),
      Tile.solved(6, 3, 4),
      Tile.solved(6, 7, 1),
      Tile.solved(7, 4, 6),
      Tile.solved(7, 6, 5),
      Tile.solved(7, 7, 2),
      Tile.solved(7, 8, 3),
      Tile.solved(8, 0, 1),
      Tile.solved(8, 2, 2),
      Tile.solved(8, 5, 9),
      Tile.solved(8, 7, 8)
    )
  )

}

package svend.sudoku

case class Coord(row: Int, col: Int) {
  def sameRow(other: Coord): Boolean = row == other.row
  def sameCol(other: Coord): Boolean = col == other.col
  def sameBox(other: Coord): Boolean =
    (row / 3 == other.row / 3) && (col / 3 == other.col / 3)

  def isPeerOf(other: Coord): Boolean = (sameBox(other) || sameCol(other) || sameCol(other)) && this != other
}

object Coord {
  def allCoords: List[Coord] = for {
    row <- (0 to 8).toList
    col <- (0 to 8)
  } yield Coord(row, col)
}

enum TileValue {
  case Solution(value: Int)
  case Pending(candidates: Set[Int])
}
import TileValue.*

case class Tile(coord: Coord, value: TileValue)

object Tile {
  def apply(row: Int, col: Int, value: Int): Tile =
    new Tile(Coord(row, col), Solution(value))

  def empty(coord: Coord): Tile =
    new Tile(coord, Pending((1 to 9).toSet))
}

opaque type Game = List[Tile]

extension (game: Game)
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
        case Solution(value) => value.toString()
        case Pending(_)      => " "
      }

      topLine + prefix + printedValue + postfix + bottomLine
    }

    grid.mkString("")
  }

  def valueAt(coord: Coord): TileValue = game.filter(_.coord == coord).head.value
  def valueAt(row: Int, col: Int): TileValue = game.valueAt(Coord(row, col))

object Game {
  def create(init: List[Tile]): Game = {
    val startCoord = init.map(_.coord)
    val blanks = Coord.allCoords
      .filter(coord => !startCoord.contains(coord))
      .map(Tile.empty)
    blanks ++ init
  }

  val game1 = create(
    List(
      Tile(4, 0, 1),
      Tile(3, 3, 7),
      Tile(0, 4, 4),
      Tile(5, 5, 8),
      Tile(7, 4, 5),
      Tile(8, 4, 6),
      Tile(4, 7, 2),
      Tile(4, 8, 3)
    )
  )

  val easy = create(
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

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

enum TileValue {
  case Solution(value: Int)
  case Pending(candidates: Set[Int])

  def isFinished = this match {
    case Solution(value)     => true
    case Pending(candidates) => false
  }

  def excludeValues(excluded: Set[Int]): TileValue = this match {
    case Solution(value) => this
    case Pending(candidates) =>
      val remaining = candidates -- excluded
      if (remaining.size == 1) Solution(remaining.head)
      else Pending(remaining)
  }

}
object TileValue {
  val validValues = (1 to 9).toSet
  val emptyTile = Pending(validValues)
}
import TileValue.*

case class Tile(coord: Coord, value: TileValue) {
  export value.isFinished
  def excludeValues(excluded: Set[Int]): Tile = copy(value = value.excludeValues(excluded))
}

object Tile {
  def apply(row: Int, col: Int, value: Int): Tile =
    new Tile(Coord(row, col), Solution(value))

  def unknown(coord: Coord): Tile =
    new Tile(coord, emptyTile)
}

opaque type Game = List[Tile]

extension (game: Game) {
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

  def replaceTile(tile: Tile): Game = game.filter(_.coord != tile.coord) :+ tile
  def peers(of: Coord): List[Tile] = game.filter(_.coord.isPeerOf(of))
  def pendingTiles = game.filter(!_.isFinished)
  def isFinished: Boolean = pendingTiles.isEmpty

}

object Game {
  def create(init: List[Tile]): Game =
    init.foldLeft(Game.empty) { (game, tile) => game.replaceTile(tile) }

  val empty = Coord.allCoords.map(Tile.unknown)

}

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

  def excludeCandidates(excluded: Set[Int]): TileValue = this match {
    case Solution(value) => this
    case Pending(candidates) =>
      val remaining = candidates -- excluded
      if (remaining.size == 1) Solution(remaining.head)
      else Pending(remaining)
  }

}
object TileValue {
  val unknownValue = Pending((1 to 9).toSet)
}
import TileValue.*

case class Tile[V <: TileValue](coord: Coord, value: V) {
  export value.isFinished
  def excludeCandidates(excluded: Set[Int]): Tile = copy(value = value.excludeCandidates(excluded))
  def excludeCandidate(excluded: Int): Tile = excludeCandidates(Set(excluded))
}

object Tile {
  def known(row: Int, col: Int, value: Int): Tile =
    new Tile(Coord(row, col), Solution(value))

  def unknownTile(coord: Coord): Tile =
    new Tile(coord, unknownValue)
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
    // this is brittle, we should check if the value is  in range
    init.foldLeft(Game.empty) { (game, tile) => game.replaceTile(tile) }

  val empty = Coord.allCoords.map(Tile.unknownTile)

  // problem that does not necessitate back-tracking, you can always deduce the next step without ambiguity
  val easy = Game.create(
    List(
      Tile.known(0, 1, 6),
      Tile.known(0, 3, 3),
      Tile.known(0, 6, 8),
      Tile.known(0, 8, 4),
      Tile.known(1, 0, 5),
      Tile.known(1, 1, 3),
      Tile.known(1, 2, 7),
      Tile.known(1, 4, 9),
      Tile.known(2, 1, 4),
      Tile.known(2, 5, 6),
      Tile.known(2, 6, 3),
      Tile.known(2, 8, 7),
      Tile.known(3, 1, 9),
      Tile.known(3, 4, 5),
      Tile.known(3, 5, 1),
      Tile.known(3, 6, 2),
      Tile.known(3, 7, 3),
      Tile.known(3, 8, 8),
      Tile.known(5, 0, 7),
      Tile.known(5, 1, 1),
      Tile.known(5, 2, 3),
      Tile.known(5, 3, 6),
      Tile.known(5, 4, 2),
      Tile.known(5, 7, 4),
      Tile.known(6, 0, 3),
      Tile.known(6, 2, 6),
      Tile.known(6, 3, 4),
      Tile.known(6, 7, 1),
      Tile.known(7, 4, 6),
      Tile.known(7, 6, 5),
      Tile.known(7, 7, 2),
      Tile.known(7, 8, 3),
      Tile.known(8, 0, 1),
      Tile.known(8, 2, 2),
      Tile.known(8, 5, 9),
      Tile.known(8, 7, 8)
    )
  )

}

package svend.sudoku

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

class ModelTest extends AnyFlatSpec with must.Matchers with ScalaCheckPropertyChecks {

  val rowGen = Gen.chooseNum(0, 8)
  val colGen = rowGen

  val coordGen = for {
    row <- rowGen
    col <- colGen
  } yield Coord(row, col)

  val tileValueGen = Gen.chooseNum(1, 9)
  val twoDifferentTileValues = for {
    v1 <- tileValueGen
    v2 <- tileValueGen.suchThat(value => value != v1)
  } yield (v1, v2)

  val solvedTileGen = for {
    coord <- coordGen
    value <- tileValueGen
  } yield Tile.solved(coord.row, coord.col, value)

  val twoNonPeerCoords = for {
    coord1 <- coordGen
    coord2 <- coordGen.suchThat(c => c.row != coord1.row && c.col != coord1.col)
  } yield (coord1, coord2)

  behavior of "Coord"

  it must "detect when 2 instances are on the same row" in
    forAll(coordGen, colGen) { (coord, col2) =>
      coord.sameRow(Coord(coord.row, col2)) mustBe (true)
    }

  it must "detect when 2 instances are on the same column" in
    forAll(coordGen, rowGen) { (coord, row2) =>
      coord.sameCol(Coord(row2, coord.col)) mustBe (true)
    }

  it must "detect when 2 instances are not on the same row nor same column" in
    forAll(twoNonPeerCoords) { (coord1, coord2) =>
      coord1.sameRow(coord2) mustBe (false)
      coord1.sameCol(coord2) mustBe (false)
    }

  it must "never be its own peer" in {
    forAll(coordGen) { coord =>
      coord.isPeerOf(coord) mustBe (false)
    }
  }

  val boxStart = Gen.oneOf(0, 3, 6)
  val delta1 = Gen.oneOf(0, 1, 2)
  val twoCoordInSameBox = for {
    boxRow <- boxStart
    boxCol <- boxStart
    rowDelta1 <- delta1
    rowDelta2 <- delta1
    colDelta1 <- delta1
    colDelta2 <- delta1
  } yield (
    Coord(boxRow + rowDelta1, boxCol + colDelta1),
    Coord(boxRow + rowDelta2, boxCol + colDelta2)
  )

  it must "detect 2 instances in the same box" in
    forAll(twoCoordInSameBox) { (coord1, coord2) =>
      coord1.sameBox(coord2) mustBe (true)
    }

  "A empty game with solved tile" must "contain the solved value in that tile" in
    forAll(solvedTileGen) { tile =>
      Game.empty.replaceTile(tile).valueAt(tile.coord.row, tile.coord.col) mustBe (tile)
    }

  behavior of "empty game"

  it must "not be finished" in {
    Game.empty.isFinished mustBe (false)
  }

  it must "not be a valid solution (since it's not solved)" in {
    Game.empty.isSolvedCorrectly mustBe (false)
  }

  it must "have all possible values as candidates in all its tiles" in
    forAll(coordGen) { coord =>
      Game.empty.valueAt(coord) mustBe (Tile.unknown(coord))
    }

  "a coordinate" must "have 20 peers" in
    forAll(coordGen) { coord =>
      Game.empty.peersOf(coord).length mustBe (20)
    }

  "A tile with an excluded candidate" must "not contain that candidate" in
    forAll(tileValueGen) { excluded =>
      Tile
        .unknown(Coord(0, 0))
        .excludeCandidate(excluded) must matchPattern {
        case Pending(_, candidates) if !candidates.contains(excluded) => ()
      }
    }

  "A pending tile with 2 candidates" must "become solved when one candidate is removed" in {
    forAll(coordGen, twoDifferentTileValues) { case (coord, (v1, v2)) =>
      Pending(coord, Set(v1, v2)).excludeCandidate(v1) must matchPattern { case Solved(coord, v2) =>
      }
    }
  }

  behavior of "game"

  it must "provide access to individual rows" in {
    (0 to 8).foreach(rowIndex =>
      val row: List[Tile] = Game.empty.row(rowIndex)
      row.foreach(tile => tile.coord.row mustBe (rowIndex))
      row.size mustBe (9)
      row.map(tile => tile.coord.col) must contain theSameElementsAs Set(0, 1, 2, 3, 4, 5, 6, 7, 8)
    )
  }

  it must "provide access to individual columns" in {
    (0 to 8).foreach(columnIndex =>
      val column: List[Tile] = Game.empty.column(columnIndex)
      column.foreach(tile => tile.coord.col mustBe (columnIndex))
      column.size mustBe (9)
      column.map(tile => tile.coord.row) must contain theSameElementsAs Set(0, 1, 2, 3, 4, 5, 6, 7, 8)
    )
  }

  it must "provide access to individurl boxes" in {

    def boxTilesCoordinates(boxRow: Int, boxCol: Int) =
      Game.empty.box(boxRow, boxCol).map(tile => tile.coord).sortBy(coord => coord.row * 10 + coord.col)

    boxTilesCoordinates(0, 0) must contain theSameElementsAs Set(
      Coord(0, 0),
      Coord(0, 1),
      Coord(0, 2),
      Coord(1, 0),
      Coord(1, 1),
      Coord(1, 2),
      Coord(2, 0),
      Coord(2, 1),
      Coord(2, 2)
    )
    boxTilesCoordinates(1, 0) must contain theSameElementsAs Set(
      Coord(3, 0),
      Coord(3, 1),
      Coord(3, 2),
      Coord(4, 0),
      Coord(4, 1),
      Coord(4, 2),
      Coord(5, 0),
      Coord(5, 1),
      Coord(5, 2)
    )
    boxTilesCoordinates(2, 0) must contain theSameElementsAs Set(
      Coord(6, 0),
      Coord(6, 1),
      Coord(6, 2),
      Coord(7, 0),
      Coord(7, 1),
      Coord(7, 2),
      Coord(8, 0),
      Coord(8, 1),
      Coord(8, 2)
    )
    boxTilesCoordinates(0, 1) must contain theSameElementsAs Set(
      Coord(0, 3),
      Coord(0, 4),
      Coord(0, 5),
      Coord(1, 3),
      Coord(1, 4),
      Coord(1, 5),
      Coord(2, 3),
      Coord(2, 4),
      Coord(2, 5)
    )
    boxTilesCoordinates(2, 2) must contain theSameElementsAs Set(
      Coord(6, 6),
      Coord(6, 7),
      Coord(6, 8),
      Coord(7, 6),
      Coord(7, 7),
      Coord(7, 8),
      Coord(8, 6),
      Coord(8, 7),
      Coord(8, 8)
    )
  }

  "all rows, all columns and all boxes" must "collectively contain the same tiles" in {

    val rowTiles: List[Tile] = Game.empty.rows.flatMap(identity)
    val columnTiles: List[Tile] = Game.empty.columns.flatMap(identity)
    val boxTiles: List[Tile] = Game.empty.boxes.flatMap(identity)

    // each tile is present in only one row, one column and one box
    rowTiles.toSet.size mustBe (rowTiles.size)
    columnTiles.toSet.size mustBe (columnTiles.size)
    boxTiles.toSet.size mustBe (boxTiles.size)

    rowTiles.toSet.size mustBe (81)
    columnTiles.toSet.size mustBe (81)
    boxTiles.toSet.size mustBe (81)

    rowTiles must contain theSameElementsAs (boxTiles)
    columnTiles must contain theSameElementsAs (boxTiles)

  }

}

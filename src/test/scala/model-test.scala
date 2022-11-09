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
  } yield Tile.known(coord.row, coord.col, value)

  "2 Coord with identical row value" must "detect that they're on the same row" in
    forAll(coordGen, colGen) { (coord, col2) =>
      coord.sameRow(Coord(coord.row, col2)) mustBe (true)
    }

  "2 Coord with identical col value" must "detect that they're on the same column" in
    forAll(coordGen, rowGen) { (coord, row2) =>
      coord.sameCol(Coord(row2, coord.col)) mustBe (true)
    }

  val twoNonPeerCoords = for {
    coord1 <- coordGen
    coord2 <- coordGen.suchThat(c => c.row != coord1.row && c.col != coord1.col)
  } yield (coord1, coord2)

  "2 Coord with different row and col value" must "detect that they are not on the same row nor same column" in
    forAll(twoNonPeerCoords) { (coord1, coord2) =>
      coord1.sameRow(coord2) mustBe (false)
      coord1.sameCol(coord2) mustBe (false)
    }

  "a Coord" must "never be its own peer" in {
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

  "2 Coord in the same box" must "be detected as such" in
    forAll(twoCoordInSameBox) { (coord1, coord2) =>
      coord1.sameBox(coord2) mustBe (true)
    }

  "All tiles in empty game" must "have all possible values in candidates" in
    forAll(coordGen) { coord =>
      Game.empty.valueAt(coord) mustBe (TileValue.unknownValue)
    }

  "A empty game with solved tile" must "contain the solved value in that tile" in
    forAll(solvedTileGen) { tile =>
      Game.empty.replaceTile(tile).valueAt(tile.coord.row, tile.coord.col) mustBe (tile.value)
    }

  "An empty game" must "not be finished" in {
    Game.empty.isFinished mustBe (false)
  }

  "a coordinate" must "have 20 peers" in
    forAll(coordGen) { coord =>
      Game.empty.peers(coord).length mustBe (20)
    }

  "A tile with an excluded candidate" must "not contain that candidate" in
    forAll(tileValueGen) { excluded =>
      Tile
        .unknownTile(Coord(0, 0))
        .excludeCandidate(excluded) must matchPattern {
        case Tile(_, TileValue.Pending(candidates)) if !candidates.contains(excluded) => ()
      }
    }

  "A pending tile with 2 candidates" must "become solved when one candidate is removed" in {
    forAll(coordGen, twoDifferentTileValues) { case (coord, (v1, v2)) =>
      Tile(coord, TileValue.Pending(Set(v1, v2))).excludeCandidate(v1) must matchPattern {
        case Tile(coord, TileValue.Solution(v2)) =>
      }
    }
  }
}

package svend.sudoku

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

class ModlTest extends AnyFlatSpec with must.Matchers with ScalaCheckPropertyChecks {

  val rowGen = Gen.chooseNum(1, 9)
  val colGen = rowGen

  val coordGen = for {
    row <- rowGen
    col <- colGen
  } yield Coord(row, col)

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

}
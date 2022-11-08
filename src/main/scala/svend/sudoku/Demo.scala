package svend.sudoku

import cats.effect.IO

object HelloWorld {

  @main def run() = {
    println("Sudoko solver")

    println(Game.easy.printableString)

  }

}

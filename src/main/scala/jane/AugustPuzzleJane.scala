package jane

import jane.PuzzleLib.Point

import scala.collection.immutable.IndexedSeq

/**
  * Created by devesh on 8/10/16.
  */
object AugustPuzzleJane extends App{
  val file = "/Users/devesh/NextStep/codejam2016/src/main/scala/jane/puzzle"

  val matrix = PuzzleLib.loadProblem(file)

  val sampleGridPoles = matrix.pivots
  val pos = matrix.startPoint

  val pivot = sampleGridPoles.head



  println(PuzzleLib.allLandingPoints(matrix.startPoint, matrix.pivots, PuzzleLib.allPoints(matrix.size).toSet, matrix))

}

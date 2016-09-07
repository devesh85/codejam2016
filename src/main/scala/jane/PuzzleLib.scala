package jane

import jane.PuzzleLib.FOURTH

import scala.collection.immutable.IndexedSeq
import scala.io.Source

/**
  * Created by devesh on 8/10/16.
  */
object PuzzleLib {
  case class Point(x: Int, y:Int)

  //Line ax + by + c = 0
  case class Line(a: Double, b: Double, c: Double)

  def getLine(p1: Point, p2: Point):Line = Line(p1.y - p2.y, p2.x - p1.x , p1.y*(p1.x-p2.x) + p1.x*(p2.y - p1.y))

  def getPerpendicularLine(line: Line, point:Point):Line = Line(line.b * line.b , -1 * line.a * line.b , line.a * line.b * point.x - line.a * line.a* point.y)

  def sq(x:Int) = x * x
  def sq(x:Double) = x * x

  def distanceSq (p1:Point, p2:Point) = sq(p1.x - p2.x) + sq(p1.y - p2.y)

  def onLine(p:Point, line:Line) = distanceFromLine(p, line) == 0

  def distanceFromLine(p: Point, line: Line): Double = {
    line.a * p.x + line.b * p.y + line.c
  }

  def isOnCircle(dSq:Int)(axis:Point)(p:Point):Boolean = dSq == distanceSq(axis, p)

  def isInCircle(dSq:Int)( axis:Point)( p:Point):Boolean = dSq > distanceSq(axis, p)

  trait Quadrant
  case object FIRST extends Quadrant
  case object SECOND extends Quadrant
  case object THIRD extends Quadrant
  case object FOURTH extends Quadrant

  def identifyQuad(point:Point, pivot:Point):Quadrant = {
    if(point.x >= pivot.x && point.y >= pivot.y) FIRST
    else if(point.x < pivot.x && point.y >= pivot.y) SECOND
    else if(point.x < pivot.x && point.y < pivot.y) THIRD
    else  FOURTH
   }

  def rotate(accessables: List[Boolean], quadOrder: List[Quadrant]):List[Quadrant] = {
     def local(bools:List[Boolean], qs:List[Quadrant], acc:List[Quadrant]):List[Quadrant] ={
       bools match {
         case x::xs => if (x) local(xs, qs.tail, qs.head::acc) else acc.reverse
         case Nil => acc.reverse
       }
     }

    local(accessables, quadOrder, List())
  }

  def accessabileQuadrants(point:Point, pivot: Point, matrix: Matrix):(List[Quadrant],List[Quadrant])={
    val currQuad = identifyQuad(point, pivot)
    val distSq = distanceSq(point, pivot)
    val radius = Math.sqrt(distSq)
    val validDist = radius + 0.5
    val isTopOk = matrix.size - 1 - (pivot.y + validDist) > 0
    val isLeftOk = (pivot.x - validDist) > 0
    val isRightOk = matrix.size - 1 -(pivot.x + validDist) > 0
    val isBottomOk = (pivot.y - validDist) > 0

    currQuad match {
      case FIRST => (FIRST::rotate(List(isTopOk, isLeftOk, isBottomOk, isRightOk), List(SECOND, THIRD,FOURTH, FIRST)),
        FIRST::rotate(List(isRightOk, isBottomOk,  isLeftOk, isTopOk), List(FOURTH, THIRD, SECOND, FIRST)))

      case SECOND => (SECOND::rotate(List(isLeftOk, isBottomOk, isRightOk,isTopOk ), List(THIRD,FOURTH, FIRST,SECOND)),
        SECOND::rotate(List(isTopOk,isRightOk,isBottomOk,isLeftOk), List(FIRST,FOURTH, THIRD, SECOND)))

      case THIRD => (THIRD::rotate(List( isBottomOk, isRightOk,isTopOk, isLeftOk), List(FOURTH, FIRST,SECOND, THIRD)),
        THIRD::rotate(List(isLeftOk, isTopOk,isRightOk, isBottomOk), List(SECOND, FIRST,FOURTH, THIRD)))

      case FOURTH => (FOURTH::rotate(List(isRightOk,isTopOk, isLeftOk, isBottomOk), List(FIRST,SECOND, THIRD,FOURTH)),
        FOURTH::rotate(List(isBottomOk,  isLeftOk, isTopOk,isRightOk), List( THIRD, SECOND, FIRST,FOURTH)))
    }
  }

  def allPoints(size:Int): IndexedSeq[Point] = for{
    x <- 0 to size
    y <- 0 to size
  }yield new Point(x, y)

  def placePointsInQuads(points:Set[Point], axis:Point, validQuds:(List[Quadrant],List[Quadrant])) ={
    ((validQuds._1.map( q => points.filter(p => identifyQuad(p, axis) == q))),
      (validQuds._2.map( q => points.filter(p => identifyQuad(p, axis) == q))))
  }

  def validPoints(start:Point, axis:Point, otherPivots:Set[Point], allPoints:Set[Point], matrix: Matrix):Set[Point] ={
    val distSq = distanceSq(start, axis)
    val circle = isOnCircle(distSq)(axis)(_)
    val insideCircle = isInCircle(distSq)(axis)(_)
    val pivotsInCircle: Set[Point] = otherPivots.filter(p => insideCircle(p)) ++ otherPivots.filter(p => circle(p))

    val allValidPoints: Set[Point] = allPoints.filter(p => circle( p))

    val allValidQuadrants = accessabileQuadrants(start, axis, matrix)

    val pointsByQuads =placePointsInQuads(allValidPoints, axis, allValidQuadrants)
    val pivotsByQuads = placePointsInQuads(pivotsInCircle, axis, allValidQuadrants)
    allValidPoints
  }

  def groupPivotsByLine(pivots:Set[Point], lines:List[Line], acc:Map[Line, Set[Point]]):Map[Line, Set[Point]] ={
       lines match{
         case x::xs => {
           val pivotsOnLine = pivots.filter(p => onLine(p, x))
           val remaining = pivots.filter(p => !onLine(p, x))
           groupPivotsByLine(remaining, xs, acc ++ Map(x -> pivotsOnLine))
         }
         case Nil => acc
       }
  }

  def pointsOnSide(point:Point, pLine:Line, points:Set[Point], fn: (Double,Double) => Boolean):Set[Point] ={
    val pointsOnSide = points.filter(p => fn(distanceFromLine(p, pLine), 0))
    if (pointsOnSide.isEmpty) Set()
    else Set(pointsOnSide.minBy(p => Math.abs(distanceSq(point, p))))
  }

  def interestingPivots(point:Point, line:Line, pivots:Set[Point]):Set[Point] ={
    val perpenicularOnPoint = getPerpendicularLine(line, point)
    pointsOnSide(point, perpenicularOnPoint, pivots, (x,y) => x >= y) ++ pointsOnSide(point, perpenicularOnPoint, pivots, (x,y) => x <= y)
  }


  def allLandingPoints(start: Point, pivots:Set[Point], all:Set[Point], matrix:Matrix):Set[Point] = {
     val allLinesFromPivots = pivots.map(p => getLine(start, p))
     val pivotsByLine = groupPivotsByLine(pivots, allLinesFromPivots.toList, Map())
     val ints: Map[Line, Set[Point]] = pivotsByLine.map {case (k,v) => k -> interestingPivots(start, k, v)}
     val pivotsOfInterest = ints.values.toSet.flatten
     pivots.map(p => validPoints(start,p, pivotsOfInterest.filter(_ != p), all, matrix)).flatten
  }

  case class Matrix( size: Int, pivots:Set[Point], startPoint: Point, endPoint: Point)

  def getPoint(str:String) = {
    val split = str.split(" ")
    new Point(split(0).toInt, split(1).toInt)
  }
  def loadProblem(file: String):Matrix = {
    val inputs: List[String] = Source.fromFile(file).getLines().toList
    val size = inputs.head.toInt
    val startPoint = getPoint(inputs.tail.head)
    val endPoint = getPoint(inputs.tail.tail.head)
    val restInputs = inputs.tail.tail.tail
    val numberOfPivots = restInputs.head.toInt
    val pivotsStr = restInputs.tail.take(numberOfPivots)
    val pivotsS = pivotsStr.map(str => getPoint(str))
    new Matrix(size, pivotsS.toSet, startPoint, endPoint)
  }

}

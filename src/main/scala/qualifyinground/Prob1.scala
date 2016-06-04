package qualifyinground

import scala.io.Source

/**
 * Created by devesh on 4/9/16.
 */
object Prob1 extends App{

  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/prob1.large"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  val inputs = list.tail.map(a  => a.toInt)

  def digitsInNum(n: Int, acc:Set[Int]): Set[Int] = {
      n match {
        case 0 => acc
        case _ => digitsInNum(n/10, acc + n % 10)
      }
  }

  def processInput(n : Int): String = {
    def lastNumSeen( a: Int, multiple: Int,currSet:Set[Int], lastNum:Int):Int = {
      if(a == 0) -1
      else if(currSet.size == 10) lastNum
      else lastNumSeen( a , multiple +1, currSet ++ digitsInNum(a * multiple, currSet), a * multiple)
    }

    val x = lastNumSeen(n, 1, Set(), n)
    if(x == -1 ) "INSOMNIA"
    else x.toString
  }

  def printCases(cases:List[Int], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

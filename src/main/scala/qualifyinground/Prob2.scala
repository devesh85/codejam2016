package qualifyinground

import scala.io.Source

/**
 * Created by devesh on 4/9/16.
 */
object Prob2 extends App{

  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/prob2.large.2"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  val inputs = list.tail.map(a => a.toCharArray.toList)


  def processInput(x:List[Char]): Int = {

    def numberOfFlips(charList: List[Char], isHappy: Boolean, flipCount:Int):Int = {
      charList match {
        case c::cs => {
          c match {
            case '+'  => if(isHappy) numberOfFlips(cs, isHappy, flipCount)
                         else numberOfFlips(cs, true, flipCount + 1)
            case '-'  => if(isHappy)  numberOfFlips(cs, false, flipCount +1)
                         else numberOfFlips(cs, isHappy, flipCount)
            case _   => flipCount
          }
        }
        case Nil   => if(isHappy) flipCount else flipCount + 1
      }
    }
    val isFirstHappy = x.head == '+'
    numberOfFlips(x, isFirstHappy, 0)
  }

  def printCases(cases:List[List[Char]], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

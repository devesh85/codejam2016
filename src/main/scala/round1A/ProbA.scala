package round1A

import scala.io.Source

/**
 * Created by devesh on 4/15/16.
 */
object ProbA extends App{

  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/probA.small"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  val inputs = list.tail



  def processInput(str : String): String = {

    def prepaareList(cList:List[Char], acc:String):String ={
      cList match {
        case x::xs => if(x >= acc.charAt(0))
                    prepaareList(xs, x.toString + acc )
                    else
                    prepaareList(xs, acc + x.toString)
        case Nil => acc
      }
    }

    val charList = str.toCharArray.toList
    prepaareList(charList.tail, charList.head.toString)
  }

  def printCases(cases:List[String], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

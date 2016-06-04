package round1B

import scala.io.Source

/**
 * Created by devesh on 4/30/16.
 */
object ProbA extends App{
  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/proba"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  val inputs = list.tail

  val code = List(('Z', 0), ('W', 2), ('U', 4), ('X', 6), ('G', 8), ('O', 1), ('F', 5),('T', 3), ('S', 7) ,('N', 9) )

  val map = Map(
    0 -> "ZERO".toCharArray.toList,
    1 -> "ONE".toCharArray.toList,
    2 -> "TWO".toCharArray.toList,
    3 -> "THREE".toCharArray.toList,
    4 -> "FOUR".toCharArray.toList,
    5 -> "FIVE".toCharArray.toList,
    6 -> "SIX".toCharArray.toList,
    7 -> "SEVEN".toCharArray.toList,
    8 -> "EIGHT".toCharArray.toList,
    9 -> "NINE".toCharArray.toList
  )

  def containsAllChars(str:String, chars:List[Char]) :Boolean = {
     chars match {
       case c::cs => if(str.indexOf(c)> -1) containsAllChars(str.replaceFirst(c.toString(),""), cs) else false
       case Nil => true
     }
  }

  def removeCharFromString(str:String, chars:List[Char]): String ={
    chars match {
      case c::cs => removeCharFromString(str.replaceFirst(c.toString(),""), cs)
      case Nil => str
    }
  }


  def processInput(str: String, cd: List[(Char, Int)], acc:String): String = {
    cd match {
      case x::xs =>
        if(str.contains(x._1)) {
          val charList = map.get(x._2)
          charList match {
            case Some(ch) => processInput(removeCharFromString(str, ch), cd, acc + x._2)
            case None => acc
          }
        }
        else processInput(str, xs, acc)
      case Nil =>
        acc.toCharArray.toList.map(x => x.toString.toInt).sorted.mkString("")
    }

  }

  def printCases(cases:List[String], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x,code,""))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

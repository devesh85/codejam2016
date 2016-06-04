package round1B

import scala.io.Source

/**
 * Created by devesh on 4/30/16.
 */
object ProbC extends App{
  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/probc"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt



  def fixMeList(ll:List[(String, String)], setA:Set[String],setB:Set[String],start:List[(String, String)], end:List[(String, String)] ):List[(String, String)]={
      ll match {
        case x::xs =>
          if(setA.contains(x._1)) fixMeList(xs, setA + x._1, setB , start, x::end)
          else if(setB.contains(x._2)) fixMeList(xs, setA + x._1, setB + x._2, start, x::end)
          else fixMeList(xs,setA + x._1, setB + x._2, x::start, end )
        case Nil => start:::end
      }
  }

  def prepareInputs(strList:List[String], acc:List[List[(String,String)]]): List[List[(String,String)]] ={
    strList match {
      case x::xs => {
        val size = x.toInt
        val list = xs.take(size).map(s => (s.split(" ")(0),s.split(" ")(1)) )

        val remaining = xs.drop(size)

        prepareInputs(remaining, fixMeList(list, Set(),Set(), Nil, Nil)::acc)
      }
      case Nil => acc.reverse
    }
  }


  val inputs = prepareInputs(list.tail, Nil)



  def processInput(inputList: List[(String,String)]): Int = {
     def proc(ll:List[(String, String)], firstSet:Set[String], secondSet:Set[String],counter:Int):Int ={
        ll match {
          case x::xs => if(firstSet.contains(x._1) && secondSet.contains(x._2)) proc(xs, firstSet, secondSet, counter + 1)
            else{
              proc(xs, firstSet + x._1 , secondSet + x._2, counter)
          }
          case Nil => counter
        }
     }

    proc(inputList, Set(), Set(), 0)
  }

  def printCases(cases:List[List[(String,String)]], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

package round1A

import scala.io.Source

/**
 * Created by devesh on 4/15/16.
 */
object ProbB extends App{
  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/probB.large"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  def prepareInputs(inputsStr:List[String], acc:List[List[List[Int]]]):List[List[List[Int]]] ={
    inputsStr match {
      case x::xs => {
        val N = x.toInt
        val lists = xs.take(2*N -1 ).map( str => str.split(" ").toList.map(a => a.toInt))

        val remaining = xs.drop(2*N -1 )
        prepareInputs(remaining, lists::acc)
      }
      case Nil => acc.reverse
    }
  }
  val inputs = prepareInputs(list.tail, Nil)
//  val inputs = list.tail.map(a  =>   a.split(" ").toList.map(x => x.toInt))





  def compareLists(first: List[Int], second:List[Int]):Boolean = {
    first match {
      case x::xs =>{
        if(first.head == second.head)
          compareLists(first.tail, second.tail)
        else
          first.head < second.head
      }
      case Nil => true
    }
  }


  def processInput(inputs:List[List[Int]]): String = {
    val allInputs = inputs.flatten
    val missing = allInputs.groupBy(x => x).map{case(k,v) => k -> v.size}.filter( p => p._2 % 2 != 0).keySet.toList.sorted
    missing.mkString(" ")
  }

  def printCases(cases:List[List[List[Int]]], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x))
      printCases(xs, index + 1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

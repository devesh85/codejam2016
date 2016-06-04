package qualifyinground

import scala.io.Source

/**
 * Created by devesh on 4/9/16.
 */
object Prob3 extends App{

  def toBinary(n: Long): String = n match {
    case 0|1 => s"$n"
        case _   => s"${toBinary(n/2)}${n%2}"
  }
  def forBaseN(chars: List[Char],n: Long, multiple: Long ,acc:Long):Long ={
    chars match {
      case c::cs => forBaseN(cs, n, multiple * n ,acc + c.toString.toInt * multiple  )
      case Nil =>  acc
    }
  }

  def longRange(curr: Long, end: Long, incr: Long, acc: List[Long]):List[Long]={
     if(curr + incr > end ) (curr::acc).reverse
     else longRange(curr + incr, end, incr, curr::acc)
  }

  def getPrimeUnder(n: Long) = {
    require(n >= 2)
    val ol = longRange(3, n, 2, Nil) // oddList
    def pn(ol: List[Long], pl: List[Long]): List[Long] = ol match {
        case Nil => pl
        case _ if pl.exists(ol.head % _ == 0) => pn(ol.tail, pl)
        case _ => pn(ol.tail, ol.head :: pl)
      }
    pn(ol, List(2)).reverse
  }

  def findFactor(n: Long):Long ={
    if(n %2 == 0) 2
    else {

      def isDivisible(x: Long, isDivided: Boolean):Long ={
        if(isDivided) x - 2
        else if(x * x > n) -1
        else {
          isDivisible(x + 2, n % x == 0)
        }
      }
      isDivisible(3, false)
    }
  }

  def fromBinaryToBases(binary:String):List[Long] = {
    val charsList = binary.toCharArray.toList.reverse
    val allBases = (2 to 10).toList
    allBases.map(n => forBaseN(charsList, n, 1, 0))
  }

  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/prob3.small_1"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  val inputs = list.tail.map(a => a.split(" ").toList.map(x => x.toInt))

  def processInput(input: List[Int]):List[String] ={
    val N = input.head
    val J = input.tail.head

    val startBin = "1" + "0" * (N-2)
    val endBin = "1" * (N - 1)

    def getFactors(binList: List[Char], acc: List[List[String]]): List[List[String]]={
      if(acc.size == J) acc
      else {
        val currNum = forBaseN(binList.reverse, 2, 1, 0)
        val nextNum = currNum + 1
        val nextBinList = toBinary(nextNum).toCharArray.toList
        val numList = fromBinaryToBases(toBinary(currNum * 2 + 1))
        val factors = numList.map(n => findFactor(n))
        val factorsAsStr = factors.map(n => n.toString)
        if(factors.contains(-1)) getFactors( nextBinList, acc )
        else getFactors(nextBinList, (binList.mkString("") + "1"::factorsAsStr)::acc )
      }
    }
    getFactors(startBin.toCharArray.toList, Nil).map(l => l.mkString(" "))
  }


  def printCases(cases:List[List[Int]], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": \n" + processInput(x).mkString("\n"))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

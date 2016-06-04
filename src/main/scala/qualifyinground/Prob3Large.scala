package qualifyinground

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.math.BigInt

/**
 * Created by devesh on 4/9/16.
 */
object Prob3Large extends App{

//  def toBinary(n: BigInt): String = n match {
//    case 0|1 => s"$n"
//        case _   => s"${toBinary(n/2)}${n%2}"
//  }
//
  def toBinary(n: BigInt): String =
    if(n == BigInt(0) || n == BigInt(1) )
      s"$n"
    else
      s"${toBinary(n/2)}${n%2}"



  val primeFactors = new ListBuffer[BigInt]()
  primeFactors += BigInt(2)
  primeFactors += BigInt(3)
  primeFactors += BigInt(5)
  primeFactors += BigInt(7)
  primeFactors += BigInt(11)
  primeFactors += BigInt(13)

  def forBaseN(chars: List[Char],n: BigInt, multiple: BigInt ,acc:BigInt):BigInt ={
    chars match {
      case c::cs => forBaseN(cs, n, multiple * n ,acc + c.toString.toInt * multiple  )
      case Nil =>  acc
    }
  }

  def longRange(curr: BigInt, end: BigInt, incr: BigInt, acc: List[BigInt]):List[BigInt]={
     if(curr + incr > end ) (curr::acc).reverse
     else longRange(curr + incr, end, incr, curr::acc)
  }

  def getPrimeUnder(n: BigInt) = {
    require(n >= 2)
    val ol = longRange(3, n, 2, Nil) // oddList
    def pn(ol: List[BigInt], pl: List[BigInt]): List[BigInt] = ol match {
        case Nil => pl
        case _ if pl.exists(ol.head % _ == 0) => pn(ol.tail, pl)
        case _ => pn(ol.tail, ol.head :: pl)
      }
    pn(ol, List(2)).reverse
  }

  var maxPrime = primeFactors.max
  def addToPrimes(n: BigInt):Boolean = {
    if(n > maxPrime){
      val isPrime = !primeFactors.exists( p => n % p == 0)
      if(isPrime){
        primeFactors += n
        maxPrime = n
        true
      }else{
        false
      }
    }else
      false
  }


  def getFirstFactor( list: List[BigInt], b : BigInt):Option[BigInt] ={
    list match {
      case x::xs => if(b%x == 0) Some(x)
                    else getFirstFactor(xs, b)
      case Nil => None
    }
  }
  def primeFactor(b : BigInt) = {
    def loop(f:BigInt, n: BigInt): BigInt = {
      val isAdded = addToPrimes(f-1) || addToPrimes(f+1)

      if ((f + 1) * (f + 1) > n) -1
      else if(isAdded){
        getFirstFactor(primeFactors.toList, b) match {
          case Some(v) => v
          case None  =>  loop(f + 6, n)
        }

      }else{
        loop(f + 6, n)
      }
    }
    loop (BigInt(6), b)
  }

  def findFactor(n: BigInt):BigInt ={
    if(n % 2 == 0) 2
    else if (n % 3 == 0) 3
    else if ( n % 5 == 0) 5
    else if ( n % 7 == 0) 7
    else if ( n % 11 == 0) 11
    else primeFactor(n)
  }

  def fromBinaryToBases(binary:String):List[BigInt] = {
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
        if(numList.filter(p => p.isProbablePrime(1000000)).isEmpty){
          val factors = numList.map(n => findFactor(n))
          val factorsAsStr = factors.map(n => n.toString)
          if(factors.contains(-1)) getFactors( nextBinList, acc )
          else getFactors(nextBinList, (binList.mkString("") + "1"::factorsAsStr)::acc )
        }else{
          getFactors( nextBinList, acc )
        }
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

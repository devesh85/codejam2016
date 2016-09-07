/**
  * Created by devesh on 7/14/16.
  */
import scala.io.Source.stdin

//Your submission should *ONLY* use the following object name
object Problem extends App {
  def charToNum(ch: Char):Int = {
    if('0' <= ch && ch <= '9') ch - '0'
    else 10 + ch.toInt - 'A'.toInt
  }
  def whichBase(str: String):Int ={
    val chList = str.toList
    charToNum(str.max) + 1
  }

  def baseNToNum(str: String, base:Int):BigInt = {
    str.toList.foldLeft(BigInt(0))((B,A) => B * base + charToNum(A))
  }
  val first = scala.io.StdIn.readLine()
  val second = scala.io.StdIn.readLine()

  val firstBase = whichBase(first)
  val secondBase = whichBase (second)

  val firstNum = baseNToNum(first, firstBase)

  val secondNum = baseNToNum(second, secondBase)

  println(baseNToNum(first, whichBase(first)) + baseNToNum(second, whichBase(second)) )
}

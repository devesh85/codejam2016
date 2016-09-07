/**
  * Created by devesh on 7/14/16.
  */
import scala.io.Source.stdin
object Palind extends  App{
  def isPalindrome(list:List[Char]):Boolean = list == list.reverse

  def constructPalin(palin:List[Char],acc:List[Char]):List[Char]={
    if(isPalindrome(palin)) acc.reverse
    else {
       palin match {
         case x::xs => constructPalin(xs, x::acc)
         case _ => acc.reverse
       }
    }
  }
  val a = "aaa".length

  val str = "abacd".toList
  println (constructPalin(str.reverse, List()).mkString(""))

  val x: Iterator[String] = scala.io.Source.stdin.getLines
  println(str.take(4))


  val y = "aaaab".toList.toSet
  val z = "traabc".toList.toSet
  println(z.diff(y).mkString(""))
}

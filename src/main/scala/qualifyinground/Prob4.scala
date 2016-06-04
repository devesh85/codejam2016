package qualifyinground

import scala.io.Source

/**
 * Created by devesh on 4/9/16.
 */
object Prob4 extends  App{
  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/prob4.small_1"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  val inputs = list.tail.map(a => {
    val xArr = a.split(" ")
    (xArr(0).toInt, xArr(1).toInt, xArr(2).toInt)
  })



  def sequenceAtLevel(key: String, level:Int): String ={

    def generateSeq(currSeq: String, currLevel:Int ):String = {
        if(currLevel >= level)  currSeq
        else{
          val newSeq = key.toCharArray.toList.map(c => {
            if(c == 'L') currSeq
            else "G" * currSeq.length
          }).mkString("")
          generateSeq(newSeq, currLevel + 1)
        }
     }

    generateSeq(key, 1)
  }

  def allPossibleComb(n:Int):List[String] ={
    val startComb = List("L", "G")
    def combine(level: Int, acc:List[String]):List[String] ={
       if(level == n ) acc
       else
          combine( level + 1 , acc.map( str => str + "L" ) ::: acc.map(str => str + "G") )
    }

    combine(1, startComb)
  }

  def leadAtPlace(str:String, index:Int) = str.charAt(index) == 'L'
  def listPlace(str:String):List[(Int, Int)] = (0 to str.length -1).toList.map( i => if(leadAtPlace(str, i)) (i, 1) else (i, 0)).filter( p => p._2  == 1)

  def processFractals(fractals:List[String], source: Int, ignorePlaces:List[Int]):List[Int]={
    if(ignorePlaces.size > source) ignorePlaces
    else{
      val leadPlacement = fractals.foldLeft[List[(Int, Int)]](Nil)((acc, str) => listPlace(str):::acc )
      val filterIgnorePlaces = leadPlacement.filter(p => !ignorePlaces.contains(p._1))
      val countLead = filterIgnorePlaces.groupBy(_._1).map{case (k, v) => k -> v.map(_._2).sum}
      if(countLead.isEmpty)
        ignorePlaces
      else{
        val minCount:Int = countLead.values.min

        val nextIgnore = (for( (k,v) <- countLead if( v == minCount)) yield k).toList.head
        if(minCount == 1) nextIgnore::ignorePlaces
        else
          processFractals(fractals.filter( p => p.charAt(nextIgnore) != 'L'), source, nextIgnore::ignorePlaces)
      }
    }
  }



  def processInput(K: Int, C: Int, S: Int): String  ={
//    val allBaseCombination = allPossibleComb(K)
//    val allSequencesAtLevel = allBaseCombination.map( key => sequenceAtLevel(key, C))

//    val leadPlacement = allSequencesAtLevel.foldLeft[List[(Int, Int)]](Nil)((acc, str) => listPlace(str):::acc )
//
//    val countLead = leadPlacement.groupBy(_._1).map{case (k, v) => k -> v.map(_._2).sum}
//    val result = processFractals(allSequencesAtLevel, S, Nil)

//    if(result.size > S)
//    "IMPOSSIBLE"
//    else
    (1 to K).mkString(" ")
  }

  def printCases(cases:List[(Int, Int, Int)], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x._1, x._2, x._3))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

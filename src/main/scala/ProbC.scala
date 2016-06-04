import scala.io.Source

/**
 * Created by devesh on 5/8/16.
 */
object ProbC extends App{
  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/probc-sample"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  val caseInputs = list.tail

  def processInput(cl:String):String ={
    val inputs = cl.split(" ")

    val J = inputs(0)toInt
    val P = inputs(1).toInt

    val S = inputs(2).toInt
    val K = inputs(3).toInt

    val resultMap = for{
      j <- 1 to J
      p <- 1 to P
      s <- 1 to S
    }yield (String.format("%s_%s %s_%s %s_%s", j.toString, p.toString , j.toString, s.toString, p.toString, s.toString) , String.format("%s %s %s", j.toString, p.toString, s.toString))

    val newGroup = resultMap.groupBy(_._1).toMap.map{case (k, v) => k -> v.head._2}

    println(newGroup)
    ""
  }

  def printCases(cases:List[String], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(caseInputs, 1)

}

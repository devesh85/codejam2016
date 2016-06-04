import scala.collection.immutable.IndexedSeq
import scala.io.Source

/**
 * Created by devesh on 5/8/16.
 */
object ProbA extends App{
  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/proba-sample"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt

  val charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray.toList
  def prepareInputs():List[List[(String, Int)]]={
     def inputs(l:List[String], acc:List[List[(String, Int)]]):List[List[(String, Int)]]={
       l match {
         case x::xs =>{
             val N = x.toInt
             val chars = charList.take(N).map(a => a.toString)

             val s = xs.head.split(" ").toList.map(a => a.toInt)

             val tuples: List[(String, Int)] = chars zip s
//             val map = tuples.groupBy(_._1).toMap.map{ case(k,v) => k -> v.head._2}
             inputs(xs.tail, tuples::acc)
         }
         case Nil => acc.reverse
       }
     }

    inputs(list.tail, Nil)
  }

  val inputsSec = prepareInputs()


  def processInput(ll:List[(String, Int)]):String={
    val sorted = ll.sortWith( _._2 > _._2)

    def egress(curr:List[(String, Int)], acc:String):String={
      val ss =  curr.sortWith( _._2 > _._2).filter(x => x._2 != 0)

      val allOnes = ss.forall( p => p._2 == 1)
      ss match {
          case x::y::xs =>  {
            val sum = x._2 + y._2 + xs.map(p => p._2).sum

            if(allOnes && xs != Nil){
              egress((x._1, x._2 - 1)::y::xs,  acc + " " +  x._1 )
            }else{
              val diff = x._2 - y._2
              if(diff == 0){
                egress((x._1, x._2 -1)::(y._1, y._2 - 1)::xs, acc + " " + x._1 + y._1 )
              }else if(diff == 1){
                egress((x._1, x._2 - 2)::y::xs,  acc + " " +  x._1 * 2  )
              }
              else{
                val gSize = diff/2
                egress((x._1, x._2 - gSize * 2)::y::xs, acc + " " + x._1 * gSize)
              }
            }


          }

          case x::Nil => {
            if(x._2 > 1){
              val gSize = x._2/2
              egress((x._1, x._2 - gSize * 2)::Nil, acc + " " + x._1 * gSize)
            }else{
              egress(Nil, acc + " " + x._1 )
            }

          }

          case Nil => acc
        }
    }
    
    egress(ll, "")
  }

  def printCases(cases: List[List[(String, Int)]], index:Int):Unit ={
    cases match {
      case x::xs => println ("Case #" + index + ":" + processInput(x))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputsSec, 1)

}

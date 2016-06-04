import scala.io.Source

/**
 * Created by devesh on 5/3/16.
 */
object PaintHouses extends App {

  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/painthouse"
  val list = Source.fromFile(file).getLines().toList

  val colors = List("R","G", "B")
  val paintPrice = list.map{str => colors zip str.split(" ").toList.map{a => a.toInt}}

//  val paintPriceMapped = paintPrice.map ( aList => aList.groupBy(_._1).map{case(k,v) => k -> v.head._2})
//
//  println(paintPriceMapped)
//
//  val tuples = paintPriceMapped.indices.map( a => a +1) zip paintPriceMapped
//
//   println (tuples.toList)
//
//
//
//  def processInput(): Int = {
//    def findMinPrice(inputList: List[(Int, Map[String, Int])], colorOption:List[String] ):Option[Int] ={
//       inputList match {
//         case x::xs =>  for{
//           c <- colorOption
//           p <- x._2.get(c)
//           tailPrice <-
//         }yield
//         case Nil =>   0
//       }
//    }
//   findMinPrice(tuples.toList, colors)
//    0
//  }
//
//
//  print(processInput())
}

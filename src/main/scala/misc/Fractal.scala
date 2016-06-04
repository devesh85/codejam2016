package misc

/**
 * Created by devesh on 5/23/16.
 */
object Fractal extends  App{
  val n = 3



  def drawTriangles(n:Int) ={
    def recurse(currLevel:Int, acc: (List[(Int,Int)], Int)):(List[(Int,Int)], Int) = {
      val currRow = acc._2

      val newTrinagles = acc._1.foldLeft[List[(Int, Int)]](List())((B, A) => List(A, (A._1 + currRow/2 , A._2 - currRow/2), (A._1 + currRow/2 , A._2 + currRow/2)) ++ B)
      if(currLevel > 1 ) recurse(currLevel -1 , (newTrinagles, currRow/2))
      else acc
    }

    def allPrints(row:Int, leftCol:Int, rightCol: Int,  height:Int, acc:List[(Int, Int)]):List[(Int, Int)] ={
      if(height == 0) acc
      else allPrints( row + 1, leftCol - 1, rightCol + 1 , height -1 ,  (leftCol to rightCol).toList.map( i => (row , i)) ++ acc)
    }


    def getString(currIndex:Int, set:Set[Int], acc:String):String = {
      if(currIndex < 64){
        val str = if(set.contains(currIndex)) "1" else " "
        getString(currIndex + 1, set, acc + str)
      }else acc
    }


    val allTriangles = recurse(n, (List((0,32)), 32))
    val printables = allTriangles._1.map(p => allPrints(p._1, p._2, p._2, allTriangles._2, List())).flatten
    val map = printables.groupBy(_._1).map{ case (k,v) => k -> v.map(p => p._2).toSet}
    val allStrings = (0 to 31).map( i => getString(0, map.getOrElse(i, Set()), "")).mkString("\n")

    println (allStrings)
  }

  drawTriangles(n)
}

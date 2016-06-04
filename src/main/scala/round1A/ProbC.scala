package round1A

import scala.io.Source

/**
 * Created by devesh on 4/15/16.
 */
object ProbC extends  App{
  val file = "/Users/devesh/NextStep/codejam2016/src/main/resources/probC"
  val list = Source.fromFile(file).getLines().toList
  val T =  list.head.toInt




  def prepareInputs(inputsStr:List[String], acc:List[List[Int]]):List[List[Int]] ={
    inputsStr match {
      case x::xs => {
        val N = x.toInt
        val lists = xs.head.split(" ").toList.map(a => a.toInt)

        prepareInputs(xs.tail, lists::acc)
      }
      case Nil => acc.reverse
    }
  }
  val inputs = prepareInputs(list.tail, Nil)
  //  val inputs = list.tail.map(a  =>   a.split(" ").toList.map(x => x.toInt))


//  println (inputs)


  def findChain(remaining:List[Int], currChain:List[Int], currSize:Int, longestChain:Int, visited:Set[Int],allKids:Set[Int],tuples: Map[Int, Int]):Int = {
    val longChain = if(longestChain > currSize) longestChain else currSize

    currChain match {
      case c::cs => {
        val bff = tuples.get(c)
        bff match {
          case Some(b) => {
            if(visited.contains(b)){
              if(visited.size == allKids.size)
                 longChain
              else{
                val others = allKids.filter(p => !visited.contains(p))
                val chainSize =
                if(tuples.exists( p => !visited.contains(p._1) && p._2 == c ) && b != cs.head  ) currSize + 1
                else currSize

                val newLongChain = if(longChain > chainSize) longChain else chainSize

                val reverseChain = currChain.reverse
                val newVisited = visited.filter( p => !reverseChain.tail.contains(p))

                remaining match {
                  case x::xs =>findChain(xs, List(x), 1, newLongChain , newVisited + x, allKids,tuples)
                  case Nil  =>  {
                    val next = allKids.filter( p => !newVisited.contains(p)).toList.head
                    findChain(Nil, List(next), 1, newLongChain , newVisited + next, allKids,tuples)
                  }
                }
              }
            }else{
              findChain(remaining, b::currChain, currSize + 1, longChain, visited + c ,allKids, tuples)
            }

          }
          case None   => -1 // not going to happen
        }

      }
      case Nil   => remaining match {
          case x::xs =>{
            val bff = tuples.get(x)
            bff match {
              case Some(b) => findChain(xs, List(x), 1, longChain , visited + x, allKids,tuples)
              case None  => -1//not going to happen
            }

          }
          case Nil   =>{
              if(visited.size == allKids.size)
                longChain
              else{
                val next = allKids.filter( p => !visited.contains(p)).toList.head
                findChain(Nil, List(next), 1, longChain , visited + next, allKids,tuples)
              }
          }
      }

    }
  }


  def processInput(bff:List[Int]): Int = {
    val tuples: Seq[(Int, Int)] = bff.indices.map( a => a +1) zip bff
    val allKids = tuples.map(x => x._1).toSet
    val allBffs = tuples.map(x => x._2).toSet

    val starters = allKids.filter( p => !allBffs.contains(p))

    findChain(starters.toList, Nil, 0, 0, Set(), allKids, tuples.groupBy(_._1).toMap.map{case (k, v) => k -> v.head._2})

  }

  def printCases(cases:List[List[Int]], index: Int):Unit={
    cases match {
      case x::xs => println ("Case #" + index + ": " + processInput(x))
        printCases(xs, index +1)
      case Nil => println ("")
    }
  }

  printCases(inputs, 1)
}

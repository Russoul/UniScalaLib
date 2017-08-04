package russoul.lib.common

import russoul.lib.common.utils.Timer

import scala.util.Random
import russoul.lib.common.math.geometry.complex.{Leaf, Node, QuadTree}
import QuadTree._
import spire.syntax.cfor._



/**
  * Created by russoul on 03.07.2017.
  */
object Test extends App {
  val f : Int => Int = 3 + _

  (f $ 1) |> println

}

object Examples{
  def treeExample(): Unit = {
    val tree = Node(
      Leaf(1),
      Leaf(2),
      Leaf(3),
      Node( //parent is automatically set
        Leaf(4),
        Leaf(5),
        Leaf(6),
        Leaf(7)
      )
    )

    printTree(tree)

  }

  /**
    * for loops like in c (from `spire`)
    *
    * prints numbers from 10 to 1
    */
  def cLangStyleForLoops(): Unit ={
    cfor(10)(_ > 0, _ - 1){ i =>
      println(i)
    }
    println("boom !")
  }
}

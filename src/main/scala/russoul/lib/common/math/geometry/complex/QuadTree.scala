package russoul.lib.common.math.geometry.complex

import russoul.lib.common._

/**
  * Created by russoul on 19.07.2017.
  */
abstract sealed class QuadTree[T](var parent : QuadTree[T]){
  var children : Vec4[QuadTree[T]] = null //2 3  <- indices based on location
  //0 1
}

case class Node[T](c0 : QuadTree[T], c1 : QuadTree[T], c2 : QuadTree[T], c3 : QuadTree[T], par : QuadTree[T] = null) extends QuadTree[T](par){
  c0.parent = this
  c1.parent = this
  c2.parent = this
  c3.parent = this
  children = Vec4 (c0, c1, c2, c3)

}

case class Leaf[T](var value : T, par : QuadTree[T] = null) extends QuadTree[T](par)


object QuadTree{
  def printTree[T](tree : QuadTree[T], level : Int = 0) : Unit = {

    val STR = "   "
    val RENDER = "-->"
    def spaces(level : Int) : String = {
      import spire.syntax.cfor._
      val builder = new StringBuilder

      cfor(0)(_ < level, _ + 1) { _ =>
        builder ++= STR
      }

      builder.result()
    }

    val str = spaces(level) + RENDER
    str |> print

    tree match{
      case node : Node[T] =>
        "\n" |> print
        for(child <- node.children) printTree(child, level + 1)
      case leaf : Leaf[T] =>
        (" " + leaf.value) |> println
    }
  }
}
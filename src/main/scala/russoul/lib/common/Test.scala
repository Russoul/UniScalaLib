package russoul.lib.common

import russoul.lib.common.utils.Timer

import scala.util.Random
import russoul.lib.common.math.geometry.complex.{Leaf, Node, QuadTree}
import QuadTree._
import russoul.lib.common.TypeClasses.{Addable, Ring}
import spire.syntax.cfor._
import Implicits._


/**
  * Created by russoul on 03.07.2017.
  */
object Test extends App {
  //Benchmark.vecPerformance()
  Benchmark.testAddable(1,2)
  println(Float2(1,1)._0)

  Float3(1,1,1) ⨯ Float3(1,1,1)
  Float3(1,1,1) cross Float3(1,1,1)
  Float2(1,1).⟂()
  Float2(1,1).⟂
  Float2(1,1) ⊗ Float2(1,1)
  Float2(1,1) elem Float2(1,1)
  println(Float2(1,1).x)
  println(Float2(2,1).x)

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

object Benchmark{

  import Implicits._


  case class FastVec3[@specialized A](val x : A, val y : A, val z : A){
    def +(that : FastVec3[A])(implicit ring : Ring[A]): FastVec3[A] = {
      FastVec3(this.x + that.x, this.y + that.y, this.z + that.z)
    }
  }

  def testAddable[A : Addable](a : A, b : A): A ={
    a + b //will be rewritten as 'evidence.plus(a,b)'
  }

  def vecPerformance(): Unit ={


    val warmup = 1000000
    val record = 10000

    var all = 0F
    val accum = (v : Float) => all += v

    val rand = new Random()

    Timer.benchmark("fast", warmup, record)(accum){
      val t = FastVec3(rand.nextFloat(),rand.nextFloat(),rand.nextFloat())

      (t + t).x
    }

    Timer.benchmark("slow", warmup, record)(accum){
      val t = Float3(rand.nextFloat(),rand.nextFloat(),rand.nextFloat())
      import Implicits._
      (t + t).x
    }
  }
}

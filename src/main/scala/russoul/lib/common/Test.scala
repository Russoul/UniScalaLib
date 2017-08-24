package russoul.lib.common

import russoul.lib.common.utils.Timer

import scala.util.Random
import russoul.lib.common.math.geometry.complex.{Leaf, Node, QuadTree}
import QuadTree._
import russoul.lib.common.TypeClasses.{Addable, Ring, Tensor0}
import spire.syntax.cfor._
import Implicits._
import russoul.lib.common.exp.Functional


/**
  * Created by russoul on 03.07.2017.
  */
object Test extends App {
  //Benchmark.vecPerformance()


  def c_for[@specialized A](i : A)(pred : A => Boolean)(step : A => A)(body : A => Unit) : Unit = {
    if (pred(i)){
      body(i)

      c_for(step(i))(pred)(step)(body)
    }else{
      Unit
    }
  }

  trait Printer[@tbsp T]{
    def print() : String
  }

  class AnyPrinter[T] extends Printer[T]{
    def print = "anyPrinter"
  }
  class FloatPrinter extends Printer[Float]{
    def print = "floatPrinter"
  }



  def tt[@sp(Float) T : Printer](t: T): Unit ={
    println(implicitly[Printer[T]].print())
  }

  implicit def anyPrinter[T](implicit no : NoImplicit[Printer[Float]]) = new AnyPrinter[T]
  implicit def floatPrinter = new FloatPrinter

  tt(1F)

  println(Vec3(1F,2F,3F) dot Vec3(1F,2F,3F))
  println(Vec3(1F,2F,3F) dot Vec3(1F,2F,3F))
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

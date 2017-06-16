import java.util.Random

import Russoul.lib.common.Ops.{AddableOps, CanonicalEuclideanSpaceOps}
import Russoul.lib.common.TypeClasses.{Addable, _}
import Russoul.lib.common._
import Russoul.lib.common.math.Solver
import Russoul.lib.common.math.algebra.{Mat, Vec3}
import Russoul.lib.common.math.geometry.simple.{OBBOverReal, TriangleOver}
import Russoul.lib.common.utils.Arr

import scala.collection.immutable


/**
  * Created by russoul on 07.06.2017.
  */

/*object PerformanceTest{
  implicit class Impl[@specialized A](j : A)(implicit ev : Times[A]){
    @inline def func: A = {
      var res = ev.one
      for(i <- 0 until 100000) res = ev.times(res, j)

      res
    }
  }

  trait Times[@specialized T]{
    @inline def times(a: T, b: T): T
    @inline def one : T
  }

  trait IntIsTimes extends Times[Int]{
    //TODO test inlining
    @inline override def times(a: Int, b: Int): Int = {
      a * b
    }

    val one = 1
  }

  implicit object IntIsTimes extends IntIsTimes

  def testSpec(): Unit ={
    //implicit def infixFieldLikeOps[@specialized A](x: A)(implicit ev : Integral[A]) =  new Implicit(x)



    @inline def func1(j: Int): Int ={
      var res = 1
      var i = 0
      while(i < 100000) {res += j + i;i += 1}

      res
    }

    @inline def func4(j: Int)(implicit impl : Times[Int]): Int ={
      var res = 1
      var i = 0
      while(i < 100000) {res += j + i;i += 1}

      res
    }

    case class Point3(var x: Double, var y: Double, var z: Double){
      @inline def +(that: Point3): Point3 ={
        Point3(this.x + that.x, this.y + that.y, this.z + that.z)
      }
    }

    @inline def func5(j: Int): Int ={
      var res = Point3(1,2,3)
      var i = 0
      while(i < 100000) {res += Point3(j,j,j) ;i += 1}

      res.x.toInt
    }

    @inline def func2[@specialized A](j: A)(implicit impl : Times[A]): A ={
      var res = impl.one
      var i = 1
      while(i < 100000) {res = impl.times(res, j); i += 1 }
      res
    }

    import Russoul.lib.common.TypeClasses.FloatIsFullField._
    val gram = new Mat[Float](3,3, new Arr[Float](1,0,0,0,1,0,0,0,1))
    implicit val gramMat = new GramMixin[Float]().mixin(gram)

    case class vec3(x: Float, y: Float, z: Float){
      @inline def *(that: vec3): Float ={
        this.x * that.x + this.y * that.y + this.z * that.z
      }
    }

    /*@inline def func3(j: Vec3[Float]): Float ={
      var res = 1F
      var i = 1
      while(i < 100000) {res *= j * j; i += 1 }
      res
    }*/

    var t1: Long = 0

    var time1,time2:Long = 0

    for(i <- 0 until 10000){
      t1 = System.nanoTime()
    }

    val rand = new Random()

    for(i <- 0 until 100){
      var res1,res2: Float = 0

      for(i <- 0 until 10000){
        t1 = System.nanoTime()
        res1 = func5(rand.nextInt())
        //println(res)
        time1 = System.nanoTime() - t1
      }

      /*for(i <- 0 until 10000){
        t1 = System.nanoTime()
        res2 = func4(rand.nextInt())
        //println(res)
        time2 = System.nanoTime() - t1
      }*/

      println("t1 " + time1 + "," + res1)
      println("t2 " + time2 + ", " + res2)
    }
  }
}*/




object MainTest extends App{

  import Implicits._
/*
  var k = 1000000

  val ar = new Array[Int](k)

  def func1(): Unit ={

  }




  for(i <- 0 until 100){
    var time1,time2: Long = 0
    var t1 = 0L
    val rand = new Random()

    for(i <- 0 until 100){
      t1 = System.nanoTime()
      ar.map(i => i + 1)//new collection creation !!!
      //println(res)
      time1 = System.nanoTime() - t1
    }

    for(i <- 0 until 100){
      t1 = System.nanoTime()
      for(i <- ar.indices) ar(i) = ar(i) + 1
      time2 = System.nanoTime() - t1
    }

    println("t1 " + time1)
    println("t2 " + time2)
    println(ar(0))
  }*/

  val res = Solver.findRealRootsPolynomial3(1D,12D,3D,4D)
  for(i <- 1 until res.size()){
    println(res(i))
  }



}

import java.util.Random

import Russoul.lib.common.Implicits._
import Russoul.lib.common.Int2
import Russoul.lib.common.Ops.Container2Ops
import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.math.Solver
import Russoul.lib.common.math.algebra.Vec2
import Russoul.lib.common.utils._


object MainTest extends App{



  /*class Immut private(private var data: Int) {

    //user
    def get() = data

    def +(that: Immut): Immut ={
      Immut(this.data + that.data)
    }

    def free(): Unit ={
      data = null
      Immut.free(index) = true
    }

    //...

    //private
    private var index = 0
    private def this() = {this(null)}
    private def set(data: Object) = this.data = 0
    //.......



  }
  object Immut{
    //user
    def apply(data: Int): Immut = {
      for(i <- 0 until size){//this alg is O(n), pretty bad, make a standalone thread for each pool type to find next free ?
        val free = this.free(i)
        if(free){
          pool(i).set(data)
          pool(i).index = i
          return pool(i)
        }
      }

      //no free obj available grow pool or alloc new for regular jc usage

      new Immut(data)

    }
    //.....

    //private
    private val size = 16
    //static pool
    //make it as bitmap
    private val free = new Array[Boolean](size)

    {
      for(i <- 0 until size){
        free(i) = true
      }
    }

    private val pool = new Array[Immut](size)

    {
      for(i <- 0 until size){
        pool(i) = new Immut //gen some instances
      }
    }
    //...
  }



  def testPools(): Unit =
  {
    val obj : Immut = Immut(1) //gets the object from pool
    val obj2 = obj + obj
    //useObj(obj)//uses the object, Obj fields are immutable to the user, no mutator methods are available to the user
    obj.free() //object is freed and not available to the user anymore, and return to the pool
    obj2.free()
  }*/

  import Russoul.lib.common._

  def testFunctional(): Unit =
  {
    val double = (x:Float) => x * 2F
    val sum = (a:Float, b: Float) => a + b

    3F |> double
    (3F,5F) ||> sum

    //val v1,v2 =   ...
    // (v1 |> f1, v2 |> f2) |> f3
  }

  def testPolynoms(): Unit = {
    val res1 = Solver.findRealRootsPolynomial3(1D, -3D, 21D, -19D)
    res1.foreach(println)


    val res2 = Solver.findRealRootsPolynomial4(1D, 3D, 3D, -1D, -6D)
    res2 match {
      case Some(x) => x.foreach(println)
      case None => println("no real roots !")
    }

  }

  def testNewtonMethod(): Unit = {
    val f = (x: Float) =>  (x+3)*(x+4) //x^2 + 2x - 8
    val dfdx = (x: Float) => 2*x + 2
    val x0 = -2.9F
    val solved = Solver.NewtonMethod.solve(x0, f, dfdx)
    println(solved)
  }

  def testAdhocPolymorphism(): Unit =
  {




    def sum[Con2,@specialized(Float) T](con2: Con2)(implicit i : Container2[T, Con2], group: CommutativeGroup[T]): T ={

      con2.x + con2.y
    }

    implicit val cont = new Vec2IsContainer2[Float]
    implicit val contArray = new ArrayIsContainerAny[Float]
    implicit val turpleCon = new Tuple2IsContainer2[Float]

    println(sum(Vec2(1F,2F)))
    println(sum(Array(1F,2F)))
    println(sum((1F,2F)))


  }

  testAdhocPolymorphism()
  testPolynoms()
}

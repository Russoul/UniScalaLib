import Russoul.lib.macros.Macros

import scala.collection.immutable
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Random


/*object DoomPyramid{
  class Something{
    def next(smth: Something): Something = smth
  }

  implicit class OptionWrapper[T](i:T){


    def ? = {
      Some(i)
    }


    def ?[B](op : => B) = {
      val re = Some(i)
      if(re.isDefined){
        Some(op)
      }else{
        None
      }
    }

  }

  implicit class OptionAid[T](i:Option[T]){
    def ?[B](next : => Option[B]): Option[B] ={
      if(i.isDefined){
        next
      }else{
        None
      }
    }

    def ! : T ={
      i.get
    }
  }

  def test(): Unit =
  {
    val e1 = new Something
    var e2 = null : Something
    val e3 = new Something

    //e2 = e1

    //val res = e1.?{e1.next(e2).?{e2.next(e3)}}

    println(null.?)

  }
}

object Test extends App {

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
    override def times(a: Int, b: Int): Int = {
      a * b
    }

    val one = 1
  }

  implicit object IntIsTimes extends IntIsTimes

 def testSpec: Unit ={
   //implicit def infixFieldLikeOps[@specialized A](x: A)(implicit ev : Integral[A]) =  new Implicit(x)



   @inline def func1(j: Int): Int ={
     var res = 1
     for(i <- 0 until 100000) res *= j

     res
   }
   @inline def func2[@specialized(Int) A](j: A)(implicit ev : Times[A]): A ={
     var res = ev.one
     for(i <- 0 until 100000) res = ev.times(res, j)

     res
   }



   var t1: Long = 0

   var time1,time2:Long = 0

   for(i <- 0 until 10000){
     t1 = System.nanoTime()
   }

   val rand = new Random()

   for(i <- 0 until 10){
     for(i <- 0 until 10000){
       t1 = System.nanoTime()
       val res = func1(rand.nextInt())
       //println(res)
       time1 = System.nanoTime() - t1
     }

     for(i <- 0 until 10000){
       t1 = System.nanoTime()
       val res = rand.nextInt().func
       //val res = func2(rand.nextInt())
       //println(res)
       time2 = System.nanoTime() - t1
     }

     println("t1 " + time1)
     println("t2 " + time2)
   }
 }


  DoomPyramid.test()



}*/


object Testy extends App{

  trait WithDim{
    def dim: Int
  }

  class MyDim(i : Int) extends WithDim{
    override def dim: Int = i
  }

  val myDim = new MyDim(3)

  println(Macros.ensureConstant(2))
}
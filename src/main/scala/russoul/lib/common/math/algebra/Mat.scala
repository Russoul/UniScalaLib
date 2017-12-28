package russoul.lib.common.math.algebra

import russoul.lib.common._
import shapeless.Nat
import shapeless.ops.nat.ToInt
import singleton.ops.XInt

import scala.collection.GenTraversableOnce
import scala.reflect.ClassTag

/**
  * Created by russoul on 11.07.2017.
  */
//TODO constructor should be private, but it does not compile this way due to bug with @sp
//n is not a field
@immutable class Mat[@specialized(Float,Double,Int) T : ClassTag, A1 <: XInt, A2 <: XInt] (n: Int, val m: Int) extends Traversable[T]{

  type E = T
  type N = A1
  type M = A2


  private val array = new Array[T](n * m)

  def apply(i: Int, j: Int) = array(i * m + j)
  def apply(i : Int) = array(i)

  //total number of elements
  override def size() : Int = array.length



  override def foreach[U](f: (T) => U): Unit = {
    var k = 0
    while (k < array.length){
      f +> array(k)
      k += 1
    }
  }

  def toArray = array.clone()

  override def toString : String = { //TODO probably use stringbuilder for better performance
    var str = ""

    val n = array.length / m

    for(i <- 0 until n){
      for(j <- 0 until m){
        str += this(i,j) + " "
      }
      str.dropRight(1)
      str += "\n"
    }

    s"Mat[${implicitly[ClassTag[T]].toString()}, ${n}, ${m}]\n$str"
  }

  override def hashCode(): Int = {
    var code: Int = 0xf457f00d

    for(i <- 0 until size){
      code = (code * 19) + array(i).##
    }

    code
  }


  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that : Mat[T, A1,A2] => this.hashCode() == that.hashCode() && {
        var i = 0
        while(i < size()){
          if(!(this(i) == that(i))) return false
          i += 1
        }

        true
      }
      case _ => false
    }
  }

}


object Mat{

  def apply[@specialized(Float,Double,Int) T : ClassTag, Size <: XInt](args: T*) : Mat[T,Size,Size] = {
    val n = Math.sqrt(args.size).toInt
    val result = new Mat[T,Size,Size](n, n)

    var i = 0

    while(i < n){
      var j = 0
      while(j < n){
        result.array(j + i*n) = args(j + i*n)
        j += 1
      }
      i += 1
    }

    result
  }



  def apply[@specialized(Float,Double,Int) T : ClassTag, A1 <: XInt, A2 <: XInt](n: Int, m: Int, args: T*) : Mat[T,A1,A2] = {
    val result = new Mat[T,A1,A2](n, m)

    var i = 0

    while(i < n){
      var j = 0
      while(j < m){
        result.array(j + i*m) = args(j + i*m)
        j += 1
      }
      i += 1
    }


    result
  }

}

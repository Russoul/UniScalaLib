package russoul.lib.common.math.algebra

import russoul.lib.common._
import singleton.ops.XInt

import scala.reflect.ClassTag

/**
  * Created by russoul on 11.07.2017.
  */
//TODO constructor should be private, but it does not compile this way due to bug with @sp
//n is not a field
@immutable class Mat[@specialized(Float,Double,Int) T : ClassTag, A1 <: XInt, A2 <: XInt] (n: Int, val m: Int, private[russoul] val array : Array[T]){

  type E = T
  type N = A1
  type M = A2




  //private val array = new Array[T](n * m)

  def apply(i: Int, j: Int) : T = array(i * m + j)
  def apply(i : Int) : T = array(i)


  //total number of elements
  def size() : Int = array.length



  def foreach[U](f: (T) => U): Unit = {
    var k = 0
    while (k < array.length){
      f(array(k))
      k += 1
    }
  }

  def toArray: Array[T] = array.clone()

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

  def apply[@specialized(Float,Double,Int) T : ClassTag, Size <: XInt](args: Array[T]) : Mat[T,Size,Size] = {
    val n = Math.sqrt(args.length).toInt

    val result = new Mat[T,Size,Size](n, n, args)

    result
  }

  def apply[@specialized(Float,Double,Int) T : ClassTag, A1 <: XInt, A2 <: XInt](n: Int, m: Int, args: Array[T]) : Mat[T,A1,A2] = {


    val result = new Mat[T,A1,A2](n, m, args)


    result
  }

}


@immutable class MatF[A1 <: XInt, A2 <: XInt] (n: Int, val m: Int, private[russoul] val array : Array[Float]){

  type E = Float
  type N = A1
  type M = A2




  //private val array = new Array[T](n * m)

  def apply(i: Int, j: Int) : Float = array(i * m + j)
  def apply(i : Int) : Float = array(i)


  //total number of elements
  def size() : Int = array.length



  def foreach[U](f: (Float) => U): Unit = {
    var k = 0
    while (k < array.length){
      f(array(k))
      k += 1
    }
  }

  def toArray: Array[Float] = array.clone()

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

    s"Mat[Float, ${n}, ${m}]\n$str"
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
      case that : MatF[A1,A2] => this.hashCode() == that.hashCode() && {
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


object MatF{

  def apply[Size <: XInt](args: Array[Float]) : MatF[Size,Size] = {
    val n = Math.sqrt(args.length).toInt

    val result = new MatF[Size,Size](n, n, args)

    result
  }

  def apply[A1 <: XInt, A2 <: XInt](n: Int, m: Int, args: Array[Float]) : MatF[A1,A2] = {


    val result = new MatF[A1,A2](n, m, args)


    result
  }

}
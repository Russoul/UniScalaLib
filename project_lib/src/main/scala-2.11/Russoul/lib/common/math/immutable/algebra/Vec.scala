package Russoul.lib.common.math.immutable.algebra

import Russoul.lib.common.lang.NotNothing
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._

import scala.reflect.ClassTag

/**
  * Created by russoul on 20.05.17.
  */

//TODO compile time scalameta checks of validity (VecDim == arg.length and others like vec(2.dim, ...) + vec(3.dim, ...))
case class Vec[A <: Dim : NotNothing, @specialized B](dim: A, array:Array[B])(implicit tagA: ClassTag[A], tagB: ClassTag[B], ev: FieldLike[B]){

  @inline def x: B = array(0)
  @inline def y: B = array(1)
  @inline def z: B = array(2)
  @inline def w: B = array(3)

  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): B = {
    array(index-1)
  }

  @inline def *[C <: Dim : NotNothing](vec: Vec[C,B]): B = {
    var res:B = ev.zero

    for(i <- array.indices){
      res += array(i) * vec.array(i)
    }

    res
  }

  @inline def ??*[C <: Dim : NotNothing](vec: Vec[C,B]): Boolean = {
    dim.n == vec.dim.n
  }

  @inline def *(scalar: B): Vec[A,B] = {
    val ar = new Array[B](dim.n)
    for(i <- 0 until dim.n){
      ar(i) = array(i) * scalar
    }

    new Vec(dim, ar)
  }

  @inline def /(scalar: B): Vec[A,B] = {
    val ar = new Array[B](dim.n)
    for(i <- 0 until dim.n){
      ar(i) = array(i) / scalar
    }

    new Vec(dim, ar)
  }

  @inline def ⊗[C <: Dim : NotNothing](vec:Vec[C,B]):Vec[Dim,B] = {
    val ar = new Array[B](dim.n)
    for(i <- 0 until dim.n){
      ar(i) = array(i) * vec.array(i)
    }

    new Vec(dim, ar)
  }

  @inline def ??⊗[C <: Dim : NotNothing](vec: Vec[C,B]): Boolean = {
    dim.n == vec.dim.n
  }

  @inline def +[C <: Dim : NotNothing](vec:Vec[C,B]):Vec[Dim,B] = {
    val ar = new Array[B](dim.n)
    for(i <- 0 until dim.n){
      ar(i) = array(i) + vec.array(i)
    }

    new Vec(dim, ar)
  }


  @inline def ??+[C <: Dim : NotNothing](vec: Vec[C,B]): Boolean = {
    dim.n == vec.dim.n
  }


  @inline def -[C <: Dim : NotNothing](vec:Vec[C,B]):Vec[Dim,B] = {
    val ar = new Array[B](dim.n)
    for(i <- 0 until dim.n){
      ar(i) = array(i) - vec.array(i)
    }

    new Vec(dim, ar)
  }

  @inline def ??-[C <: Dim : NotNothing](vec: Vec[C,B]): Boolean = {
    dim.n == vec.dim.n
  }

  @inline def unary_-(): Vec[A,B] = {
    val ar = new Array[B](dim.n)
    for(i <- 0 until dim.n){
      ar(i) = -array(i)
    }

    new Vec(dim, ar)
  }

  @inline def unary_+(): Vec[A,B] = {
    val ar = new Array[B](dim.n)
    for(i <- 0 until dim.n){
      ar(i) = array(i)
    }

    new Vec(dim, ar)
  }

  /**
    * defined only for vectors of dim 2
    * @return
    */
  @inline def ⟂():Vec[Dim,B] = {
    Vec(Two, y, -x)
  }


  @inline def ??⟂():Boolean = {
    dim.n == 2
  }

  @inline def ?⟂():Option[Vec[Dim,B]] = {
    if(??⟂()) Some(Vec(Two, y, -x)) else None
  }


  /**
    * defined only for vectors of dim 3
    * @return
    */
  @inline def ⨯[C <: Dim : NotNothing](v:Vec[C,B]):Vec[Dim,B] = {
    Vec(dim, y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  }



  @inline def ??⨯[C <: Dim : NotNothing](v:Vec[C,B]):Boolean = {
    dim.n == 3 && v.dim.n == 3
  }

  @inline def ?⨯[C <: Dim : NotNothing](v:Vec[C,B]):Option[Vec[Dim,B]] = {
    if(??⨯(v)) Some(Vec(dim, y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)) else None
  }

  @inline def squaredLength():B = {
    var res:B = ev.one

    for(i <- array.indices){
      res *= array(i) * array(i)
    }

    res
  }

  @inline def length():B = {
    ev.sqrt(squaredLength())
  }

  @inline def normalize(): Vec[A,B] = {
    val ar = new Array[B](dim.n)
    val len = length()
    for(i <- 0 until dim.n){
      ar(i) = array(i) / len
    }

    new Vec(dim, ar)
  }

  override def toString(): String = {
    val bld = new StringBuilder


    bld ++= "Vec[" + dim.n + "," + ev.toString() + "]("

    for(a <- array){
      bld ++= a.toString + "; "
    }

    bld.delete(bld.length - 2, bld.length)
    bld += ')'

    bld.result()
  }
}

object Vec{
  def apply[A <: Dim : NotNothing, B](dim:A, seq: B*)(implicit tagA: ClassTag[A], tagB : ClassTag[B], ev: FieldLike[B]): Vec[A,B] = {

    val arr = new Array[B](dim.n)

    for(i <- 0 until dim.n){
      arr(i) = seq(i)
    }

    new Vec(dim, arr)
  }


  //EXTRA OPS...........................................................................
  @inline def ⟂[B : FieldLike](v:Vec[Two, B]):Vec[Two,B] = {
    Vec(Two, v.y, -v.x)
  }


  @inline def ⨯[B : FieldLike](a:Vec[Three,B], b:Vec[Three,B]):Vec[Three,B] = {
    Vec(Three, a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
  }
  //....................................................................................

}

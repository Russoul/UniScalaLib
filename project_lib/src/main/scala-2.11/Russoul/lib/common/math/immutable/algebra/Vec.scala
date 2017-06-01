package Russoul.lib.common.math.immutable.algebra

import Russoul.lib.common.Real
import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.utils.Arr
import Russoul.lib.common.math.TypeClasses.Field.Implicits._

/**
  * Created by russoul on 01.06.2017.
  */
@immutable case class Vec[F](private val array:Arr[F], var isColumn:Boolean = true)(implicit ev: Field[F]) {


  @inline @unchecked def x: F = array(0)
  @inline @unchecked def y: F = array(1)
  @inline @unchecked def z: F = array(2)
  @inline @unchecked def w: F = array(3)

  @inline def dim():Int = array.size
  @inline def size():Int = array.size

  @inline def setAsColumn(): Vec[F] = {
    isColumn = true
    this
  }

  @inline def setAsRow(): Vec[F] = {
    isColumn = false
    this
  }

  @inline @unchecked def apply(index: Int): F = {
    array(index-1)
  }

  @inline def *(scalar: F): Vec[F] = {
    val ar = new Arr[F](array.size)
    for(i <- 0 until array.size){
      ar(i) = array(i) * scalar
    }

    new Vec(ar)
  }

  @inline def /(scalar: F): Vec[F] = {
    val ar = new Arr[F](array.size)
    for(i <- 0 until array.size){
      ar(i) = array(i) / scalar
    }

    new Vec(ar)
  }


  @inline @unchecked def +(that: Vec[F]):Vec[F] = {
    val ar = new Arr[F](array.size)
    for(i <- 0 until array.size){
      ar(i) = this.array(i) + that.array(i)
    }

    new Vec(ar)
  }

  @inline @unchecked def -(that: Vec[F]):Vec[F] = {
    val ar = new Arr[F](array.size)
    for(i <- 0 until array.size){
      ar(i) = this.array(i) - that.array(i)
    }

    new Vec(ar)
  }

  @inline def ??+(that: Vec[F]): Boolean = {
    this.array.size == that.array.size
  }

  @inline def ??-(that: Vec[F]): Boolean = {
    this.array.size == that.array.size
  }

  @inline @unchecked def unary_-():Vec[F] = {
    val ar = new Arr[F](array.size)
    for(i <- 0 until array.size){
      ar(i) = -this.array(i)
    }

    new Vec(ar)
  }

  @inline def ??⨯(that:Mat[F]): Boolean ={
    if(isColumn) that.rows == 1 else that.rows == dim
  }

  @inline @unchecked def ⨯(that:Mat[F]): Mat[F] ={
    val p = that.columns

    if(isColumn){
      val ar = new Arr[F](dim * p)
      val res = new Mat[F](dim, p, ar)

      for(i <- 1 to dim){
        for(j <- 1 to p){
          res(i)(j) += this(i) * that(1)(j)
        }
      }

      res
    }else{
      val ar = new Arr[F](1 * p)
      val res = new Mat[F](1, p, ar)

      for(j <- 1 to p){
        for(k <- 1 to dim) {
          res(1)(j) += this(k) * that(k)(j)
        }
      }

      res
    }
  }

}

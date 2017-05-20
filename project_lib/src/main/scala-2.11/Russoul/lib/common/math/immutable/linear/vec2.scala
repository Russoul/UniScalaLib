package Russoul.lib.common.math.immutable.linear


import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike

import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._
/**
  *
  * immutable
  */
@immutable case class vec2[@specialized A](array:Array[A])(implicit ev:FieldLike[A]) {


  @inline def x: A = array(0)

  @inline def y: A = array(1)

  private def this(dx:A,dy:A){
    this(Array(dx,dy))
  }


  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): A = {
    array(index-1)
  }


  @inline def *(vec: vec2[A]) = {
    this (1) * vec(1) + this (2) * vec(2)
  }

  //by element product
  @inline def **(vec:vec2[A]):vec2[A] = {
    vec2(this.x*vec.x, this.y*vec.y)
  }

  @inline def dotProduct(vec: vec2[A]): A = {
    this (1) * vec(1) + this (2) * vec(2)
  }


  @inline def *(scalar: A): vec2[A] = {
    vec2(this (1) * scalar, this (2) * scalar)
  }

  @inline def /(scalar: A): vec2[A] = {
    vec2(this (1) / scalar, this (2) / scalar)
  }

  @inline def scalarMultiplication(scalar: A): vec2[A] = {
    vec2(this (1) * scalar, this (2) * scalar)
  }

  @inline def add(vec: vec2[A]): vec2[A] = {
    vec2(this (1) + vec(1), this (2) + vec(2))
  }

  @inline def subtract(vec: vec2[A]): vec2[A] = {
    vec2(this (1) - vec(1), this (2) - vec(2))
  }

  @inline def -(vec: vec2[A]): vec2[A] = {
    vec2(this (1) - vec(1), this (2) - vec(2))
  }

  @inline def +(vec: vec2[A]): vec2[A] = {
    vec2(this (1) + vec(1), this (2) + vec(2))
  }

  @inline def unary_-(): vec2[A] = {
    vec2(-this(1), -this(2))
  }

  @inline def vec2OrthogonalToThisOneToTheRight():vec2[A] = {
    vec2(y,-x)
  }

  override def toString(): String = {
     "vec2( " + x + "; " + y + " )"
  }


  @inline def length(): A = {
    val r = x * x + y * y
    ev.sqrt(r)
  }

  @inline def squareLength(): A = {
    x * x + y * y
  }



  def toArray2f(): Array[A] = {
    Array(this (1), this (2))
  }



  @inline def normalize(): vec2[A] = {
    this * (ev.one / length())
  }


  def toSeq2(): Seq[A] = {
    Seq(x, y)
  }






}

object vec2 {
  def apply[@specialized A : Fractional](x: A, y: A) = new vec2(x,y)
}


/*override def hashCode(): Int =
  {

    var res = 1
    val a = java.lang.Float.floatToIntBits(x)
    val b = java.lang.Float.floatToIntBits(y)

    res += 37 * res + a
    res += 37 * res + b

    res
  }*/
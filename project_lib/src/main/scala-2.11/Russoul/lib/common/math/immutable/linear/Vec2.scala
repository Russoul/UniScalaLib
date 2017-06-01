package Russoul.lib.common.math.immutable.linear


import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.{Field, Euclidean}
import Russoul.lib.common.math.TypeClasses.Field.Implicits._
/**
  *
  * immutable
  */
@immutable case class Vec2[@specialized A](array:Array[A])(implicit ev:Field[A], pow:Euclidean[A]) {


  @inline def x: A = array(0)

  @inline def y: A = array(1)

  private def this(dx:A,dy:A){
    this(Array(dx,dy))
  }


  @inline def add(vec: Vec2[A]): Vec2[A] = {
    this.+(vec)
  }

  @inline def subtract(vec: Vec2[A]): Vec2[A] = {
    this.-(vec)
  }

  @inline def dotProduct(vec: Vec2[A]): A = {
    *(vec)
  }

  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): A = {
    array(index-1)
  }


  @inline def *(vec: Vec2[A]): A = {
    this (1) * vec(1) + this (2) * vec(2)
  }

  //by element product
  @inline def ⊗(vec:Vec2[A]):Vec2[A] = {
    Vec2(this.x*vec.x, this.y*vec.y)
  }

  //by element product
  @inline def **(vec:Vec2[A]):Vec2[A] = {
    ⊗(vec)
  }

  @inline def *(scalar: A): Vec2[A] = {
    Vec2(this (1) * scalar, this (2) * scalar)
  }

  @inline def /(scalar: A): Vec2[A] = {
    Vec2(this (1) / scalar, this (2) / scalar)
  }

  @inline def scalarMultiplication(scalar: A): Vec2[A] = {
    *(scalar)
  }


  @inline def -(vec: Vec2[A]): Vec2[A] = {
    Vec2(this (1) - vec(1), this (2) - vec(2))
  }

  @inline def +(vec: Vec2[A]): Vec2[A] = {
    Vec2(this (1) + vec(1), this (2) + vec(2))
  }

  @inline def unary_-(): Vec2[A] = {
    Vec2(-this(1), -this(2))
  }

  @inline def vec2OrthogonalToThisOneToTheRight():Vec2[A] = {
    ⟂()
  }

  @inline def ⟂():Vec2[A] = {
    Vec2(y, -x)
  }

  override def toString(): String = {
     "vec2( " + x + "; " + y + " )"
  }


  @inline def length(): A = {
    val r = x * x + y * y
    pow.sqrt(r)
  }



  @inline def squareLength(): A = {
    x * x + y * y
  }



  def toArray2f(): Array[A] = {
    Array(this (1), this (2))
  }



  @inline def normalize(): Vec2[A] = {
    this * (ev.one / length())
  }


  def toSeq2(): Seq[A] = {
    Seq(x, y)
  }






}

object Vec2 {
  def apply[@specialized A : Fractional](x: A, y: A) = new Vec2(x,y)
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
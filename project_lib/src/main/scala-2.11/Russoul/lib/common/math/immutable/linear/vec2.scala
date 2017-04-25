package Russoul.lib.common.math.immutable.linear

import java.nio.FloatBuffer



/**
  *
  * immutable
  */
class vec2(arrayIn:Array[Float])
{

  protected[lib] val array: Array[Float] = arrayIn

  @inline def x: Float = array(0)

  @inline def y: Float = array(1)

  def this(dx:Float,dy:Float){
    this(Array(dx,dy))
  }


  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): Float =
  {
    array(index-1)
  }


  @inline def *(vec: vec2) =
  {
    this (1) * vec(1) + this (2) * vec(2)
  }

  //by element product
  @inline def **(vec:vec2):vec2 =
  {
    vec2(this.x*vec.x, this.y*vec.y)
  }

  @inline def dotProduct(vec: vec2): Float =
  {
    this (1) * vec(1) + this (2) * vec(2)
  }


  @inline def *(scalar: Float): vec2 =
  {
    vec2(this (1) * scalar, this (2) * scalar)
  }

  @inline def /(scalar: Float): vec2 =
  {
    vec2(this (1) / scalar, this (2) / scalar)
  }

  @inline def scalarMultiplication(scalar: Float): vec2 =
  {
    vec2(this (1) * scalar, this (2) * scalar)
  }

  @inline def add(vec: vec2): vec2 =
  {
    vec2(this (1) + vec(1), this (2) + vec(2))
  }

  @inline def subtract(vec: vec2): vec2 =
  {
    vec2(this (1) - vec(1), this (2) - vec(2))
  }

  @inline def -(vec: vec2): vec2 =
  {
    vec2(this (1) - vec(1), this (2) - vec(2))
  }

  @inline def +(vec: vec2): vec2 =
  {
    vec2(this (1) + vec(1), this (2) + vec(2))
  }

  @inline def unary_-(): vec2 =
  {
    this * (-1)
  }

  @inline def vec2OrthogonalToThisOneToTheRight():vec2 =
  {
    vec2(y,-x)
  }

  override def toString(): String =
  {
     "vec2( " + x + "; " + y + " )"
  }


  @inline def length(): Float =
  {
    val r = x * x + y * y
    math.sqrt(r).toFloat
  }

  @inline def squareLength(): Float =
  {
    x * x + y * y
  }



  def toArray2f(): Array[Float] =
  {
    Array(this (1), this (2))
  }


  @inline def copy(): vec2 =
  {
    vec2(this (1), this (2))
  }

  @inline def normalize(): vec2 =
  {
    this * (1 / length())
  }


  def toSeq2(): Seq[Float] =
  {
    Seq(x, y)
  }



  override def equals(obj: scala.Any): Boolean =
  {
    obj match {
      case v: vec2 =>
        return x == v.x && y == v.y
      case _ =>
    }
    false
  }

  override def hashCode(): Int =
  {

    var res = 1
    val a = java.lang.Float.floatToIntBits(x)
    val b = java.lang.Float.floatToIntBits(y)

    res += 37 * res + a
    res += 37 * res + b

    res
  }


}

object vec2
{

  def apply(x: Float, y: Float) = new vec2(x,y)

  def newSpace() = new Array[Float](2)

  def getByteSize() = 8
}
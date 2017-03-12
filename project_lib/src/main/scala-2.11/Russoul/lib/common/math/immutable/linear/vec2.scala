package Russoul.lib.common.math.immutable.linear

import java.nio.FloatBuffer



/**
  *
  * immutable
  */
class vec2(arrayIn:Array[Float])
{

  protected[lib] val array = arrayIn

  def x = array(0)

  def y = array(1)

  def this(dx:Float,dy:Float){
    this(Array(dx,dy))
  }


  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  def apply(index: Int): Float =
  {
    array(index-1)
  }


  def *(vec: vec2) =
  {
    dotProduct(vec)
  }

  def dotProduct(vec: vec2): Float =
  {
    this (1) * vec(1) + this (2) * vec(2)
  }


  def *(scalar: Float): vec2 =
  {
    scalarMultiplication(scalar)
  }

  def scalarMultiplication(scalar: Float): vec2 =
  {
    vec2(this (1) * scalar, this (2) * scalar)
  }

  def add(vec: vec2): vec2 =
  {
    vec2(this (1) + vec(1), this (2) + vec(2))
  }

  def subtract(vec: vec2): vec2 =
  {
    add(-vec)
  }

  def -(vec: vec2): vec2 =
  {
    subtract(vec)
  }

  def +(vec: vec2): vec2 =
  {
    add(vec)
  }

  def unary_-(): vec2 =
  {
    this * (-1)
  }



  override def toString(): String =
  {
    var re = ""
    for (i <- 1 to 2) {
      re += (apply(i) + (if (i != 2) " " else ""))
    }
    re
  }


  def length(): Float =
  {
    val r = x * x + y * y
    math.sqrt(r).toFloat
  }

  def squareLength(): Float =
  {
    x * x + y * y
  }



  def toArray2f(): Array[Float] =
  {
    Array(this (1), this (2))
  }


  def copy(): vec2 =
  {
    vec2(this (1), this (2))
  }

  def normalize(): vec2 =
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
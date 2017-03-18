package Russoul.lib.common.math.immutable.linear

import java.nio.{ByteBuffer, FloatBuffer}


import scala.language.implicitConversions


/**
  *
  * immutable
  */



class vec3(arrayIn:Array[Float])
{



  protected[lib] val array = arrayIn

  def x = array(0)

  def y = array(1)

  def z = array(2)


  def this(dx:Float,dy:Float,dz:Float){
    this(Array[Float](dx,dy,dz))
  }

  def <(right:vec3):Boolean =
  {
    if (x < right.x)
      return true
    else if (x > right.x)
      return false

    if (y < right.y)
      return true
    else if (y > right.y)
      return false

    if (z < right.z)
      return true
    else if (z > right.z)
      return false

    false
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


  def *(vec: vec3) =
  {
    dotProduct(vec)
  }

  def *(mat:mat4) =
  {
    (vec4(this,1) * mat).toVec3()
  }

  //by element product
  def **(vec:vec3):vec3 =
  {
    vec3(this.x*vec.x, this.y*vec.y, this.z*vec.z)
  }

  def dotProduct(vec: vec3): Float =
  {
    this (1) * vec(1) + this (2) * vec(2) + this (3) * vec(3)
  }


  def *(scalar: Float): vec3 =
  {
    vec3(x * scalar, y * scalar, z * scalar)
  }
  def /(scalar:Float):vec3 =
  {
    vec3(x / scalar, y / scalar, z / scalar)
  }

  def scalarMultiplication(scalar: Float): vec3 =
  {
    vec3(this (1) * scalar, this (2) * scalar, this (3) * scalar)
  }

  def add(vec: vec3): vec3 =
  {
    vec3(x + vec.x, y + vec.y, z + vec.z)
  }

  def subtract(vec: vec3): vec3 =
  {
    vec3(x - vec.x, y - vec.y, z - vec.z)
  }

  def -(vec: vec3): vec3 =
  {
    vec3(x - vec.x, y - vec.y, z - vec.z)
  }

  def +(vec: vec3): vec3 =
  {
    vec3(x + vec.x, y + vec.y, z + vec.z)
  }

  def unary_-(): vec3 =
  {
    this * (-1)
  }

  def ^(vec:vec3): vec3 =
  {
    this.crossProduct(vec)
  }


  override def toString(): String =
  {
    var re = ""
    for (i <- 1 to 3) {
      re += (apply(i) + (if (i != 3) " " else ""))
    }
    re
  }


  def length(): Float =
  {
    val r = x * x + y * y + z * z
    math.sqrt(r).toFloat
  }

  def squareLength(): Float =
  {
    x * x + y * y + z * z
  }



  def toArray2f(): Array[Float] =
  {
    Array(this (1), this (2))
  }

  def toArray3f(): Array[Float] =
  {
    Array(this (1), this (2), this (3))
  }


  def copy(): vec3 =
  {
    vec3(this (1), this (2), this (3))
  }

  def normalize(): vec3 =
  {
    this * (1 / length())
  }

  def wOne(): vec4 = vec4(this (1), this (2), this (3), 1)

  def wZero(): vec4 = vec4(this (1), this (2), this (3), 0)

  def xy() = vec2(x,y)

  def crossProduct(v: vec3): vec3 =
  {
    vec3(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  }

  def toSeq2(): Seq[Float] =
  {
    Seq(x, y)
  }

  def toSeq3(): Seq[Float] =
  {
    Seq(x, y, z)
  }


  override def equals(obj: scala.Any): Boolean =
  {
    obj match {
      case v: vec3 =>
        equals(v)
      case _ => false
    }
  }

  def equals(v: vec3): Boolean =
  {
    x == v.x && y == v.y && z == v.z
  }

  override def hashCode(): Int =
  {

    var res = 1
    val a = java.lang.Float.floatToIntBits(x)
    val b = java.lang.Float.floatToIntBits(y)
    val c = java.lang.Float.floatToIntBits(z)

    res += 37 * res + a
    res += 37 * res + b
    res += 37 * res + c

    res
  }



  def size(): Int = 3

}



object vec3
{

  def apply(x: Float, y: Float, z: Float) = new vec3(x,y,z)
  def apply(array:Array[Float]) = new vec3(array)

  def newSpace() = new Array[Float](3)

  def getByteSize() = 12

  def apply(v2:vec2, z:Float) = new vec3(v2.x, v2.y, z)

}




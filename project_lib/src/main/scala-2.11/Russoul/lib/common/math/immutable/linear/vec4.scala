package Russoul.lib.common.math.immutable.linear

import Russoul.lib.common.lang.immutable


/**
  *
  *
  * immutable
  */
@immutable case class vec4(array:Array[Float]) {

  @inline def x: Float = array(0)

  @inline def y: Float = array(1)

  @inline def z: Float = array(2)

  @inline def w: Float = array(3)

  private def this(dx:Float, dy:Float, dz:Float, dw:Float)
  {
    this(Array[Float](dx,dy,dz,dw))
  }


  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): Float = {
    array(index-1)
  }


  @inline def *(vec: vec4): Float = {
    this (1) * vec(1) + this (2) * vec(2) + this (3) * vec(3) + this (4) * vec(4)
  }

  @inline def dotProduct(vec: vec4): Float = {
    this (1) * vec(1) + this (2) * vec(2) + this (3) * vec(3) + this (4) * vec(4)
  }

  @inline def matrixProduct(matrix: mat4): vec4 = {
    vec4(this * matrix.column(1), this * matrix.column(2), this * matrix.column(3), this * matrix.column(4))
  }

  @inline def *(matrix: mat4): vec4 = {
    vec4(this * matrix.column(1), this * matrix.column(2), this * matrix.column(3), this * matrix.column(4))
  }

  @inline def *(scalar: Float): vec4 = {
    vec4(this (1) * scalar, this (2) * scalar, this (3) * scalar, this (4) * scalar)
  }

  @inline def **(vec: vec4): vec4 = {
    vec4(this (1) * vec(1), this (2) * vec(2), this (3) * vec(3), this (4) * vec(4))
  }

  @inline def /(scalar: Float): vec4 = {
    vec4(this (1) / scalar, this (2) / scalar, this (3) / scalar, this (4) / scalar)
  }

  @inline def scalarMultiplication(scalar: Float): vec4 = {
    vec4(this (1) * scalar, this (2) * scalar, this (3) * scalar, this (4) * scalar)
  }

  @inline def add(vec: vec4): vec4 = {
    vec4(this (1) + vec(1), this (2) + vec(2), this (3) + vec(3), this (4) + vec(4))
  }

  @inline def subtract(vec: vec4): vec4 = {
    vec4(this (1) - vec(1), this (2) - vec(2), this (3) - vec(3), this (4) - vec(4))
  }

  @inline def -(vec: vec4): vec4 = {
    vec4(this (1) - vec(1), this (2) - vec(2), this (3) - vec(3), this (4) - vec(4))
  }

  @inline def +(vec: vec4): vec4 = {
    vec4(this (1) + vec(1), this (2) + vec(2), this (3) + vec(3), this (4) + vec(4))
  }

  @inline def unary_-(): vec4 = {
    this * (-1)
  }

  def toMatrix4FirstColumn(): mat4 = {
    mat4(this (1), 0, 0, 0,
      this (2), 0, 0, 0,
      this (3), 0, 0, 0,
      this (4), 0, 0, 0)
  }

  override def toString(): String = {
    "vec4( " + x + "; " + y + "; " + z + "; " + w + " )"
  }


  @inline def length(): Float = {
    math.sqrt(squareLength()).toFloat
  }

  @inline def squareLength(): Float = {
    x * x + y * y + z * z + w * w
  }



  def toArray2f(): Array[Float] = {
    Array(this (1), this (2))
  }

  def toArray3f(): Array[Float] = {
    Array(this (1), this (2), this (3))
  }

  def toArray4f(): Array[Float] = {
    Array(this (1), this (2), this (3), this (4))
  }


  @inline def normalize(): vec4 = {
    this * (1 / length())
  }

  @inline def wOne(): vec4 = vec4(this (1), this (2), this (3), 1)

  @inline def wZero(): vec4 = vec4(this (1), this (2), this (3), 0)


  @inline def divideByW(): vec4 = this * (1 / w)


  def toSeq2(): Seq[Float] = {
    Seq(x, y)
  }

  def toSeq3(): Seq[Float] = {
    Seq(x, y, z)
  }

  def toSeq4(): Seq[Float] = {
    Seq(x, y, z, w)
  }

  def toVec3():vec3 = {
    vec3(x,y,z)
  }

  @inline def xyz(): vec3 = vec3(x,y,z)


}

object vec4 {
  def apply(x: Float, y: Float, z: Float, w: Float) = new vec4(x,y,z,w)
  def apply(v:vec3, w:Float): vec4 = new vec4(v.x, v.y, v.z, w)
}


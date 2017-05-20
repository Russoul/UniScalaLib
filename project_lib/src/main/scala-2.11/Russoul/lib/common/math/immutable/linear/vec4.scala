package Russoul.lib.common.math.immutable.linear

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike

import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._
/**
  *
  *
  * immutable
  */
@immutable case class vec4[@specialized A](array:Array[A])(implicit ev: FieldLike[A]) {

  @inline def x: A = array(0)

  @inline def y: A = array(1)

  @inline def z: A = array(2)

  @inline def w: A = array(3)

  private def this(dx:A, dy:A, dz:A, dw:A)
  {
    this(Array[A](dx,dy,dz,dw))
  }


  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): A = {
    array(index-1)
  }


  @inline def *(vec: vec4[A]): A = {
    this (1) * vec(1) + this (2) * vec(2) + this (3) * vec(3) + this (4) * vec(4)
  }

  @inline def dotProduct(vec: vec4[A]): A = {
    this (1) * vec(1) + this (2) * vec(2) + this (3) * vec(3) + this (4) * vec(4)
  }

  @inline def matrixProduct(matrix: mat4[A]): vec4[A] = {
    vec4(this * matrix.column(1), this * matrix.column(2), this * matrix.column(3), this * matrix.column(4))
  }

  @inline def *(matrix: mat4[A]): vec4[A] = {
    vec4(this * matrix.column(1), this * matrix.column(2), this * matrix.column(3), this * matrix.column(4))
  }

  @inline def *(scalar: A): vec4[A] = {
    vec4(this (1) * scalar, this (2) * scalar, this (3) * scalar, this (4) * scalar)
  }

  @inline def **(vec: vec4[A]): vec4[A] = {
    vec4(this (1) * vec(1), this (2) * vec(2), this (3) * vec(3), this (4) * vec(4))
  }

  @inline def /(scalar: A): vec4[A] = {
    vec4(this (1) / scalar, this (2) / scalar, this (3) / scalar, this (4) / scalar)
  }

  @inline def scalarMultiplication(scalar: A): vec4[A] = {
    vec4(this (1) * scalar, this (2) * scalar, this (3) * scalar, this (4) * scalar)
  }

  @inline def add(vec: vec4[A]): vec4[A] = {
    vec4(this (1) + vec(1), this (2) + vec(2), this (3) + vec(3), this (4) + vec(4))
  }

  @inline def subtract(vec: vec4[A]): vec4[A] = {
    vec4(this (1) - vec(1), this (2) - vec(2), this (3) - vec(3), this (4) - vec(4))
  }

  @inline def -(vec: vec4[A]): vec4[A] = {
    vec4(this (1) - vec(1), this (2) - vec(2), this (3) - vec(3), this (4) - vec(4))
  }

  @inline def +(vec: vec4[A]): vec4[A] = {
    vec4(this (1) + vec(1), this (2) + vec(2), this (3) + vec(3), this (4) + vec(4))
  }

  @inline def unary_-(): vec4[A] = {
    this * (-ev.one)
  }

  def toMatrix4FirstColumn(): mat4[A] = {
    
    
    
    mat4(this (1), ev.zero, ev.zero, ev.zero,
      this (2), ev.zero, ev.zero, ev.zero,
      this (3), ev.zero, ev.zero, ev.zero,
      this (4), ev.zero, ev.zero, ev.zero)
  }

  override def toString(): String = {
    "vec4( " + x + "; " + y + "; " + z + "; " + w + " )"
  }


  @inline def length(): A = {
    ev.sqrt(squareLength())
  }

  @inline def squareLength(): A = {
    x * x + y * y + z * z + w * w
  }



  def toArray2f(): Array[A] = {
    Array(this (1), this (2))
  }

  def toArray3f(): Array[A] = {
    Array(this (1), this (2), this (3))
  }

  def toArray4f(): Array[A] = {
    Array(this (1), this (2), this (3), this (4))
  }


  @inline def normalize(): vec4[A] = {
    this * (ev.one / length())
  }

  @inline def wOne(): vec4[A] = vec4(this (1), this (2), this (3), ev.one)

  @inline def wZero(): vec4[A] = vec4(this (1), this (2), this (3), ev.zero)


  @inline def divideByW(): vec4[A] = this * (ev.one / w)


  def toSeq2(): Seq[A] = {
    Seq(x, y)
  }

  def toSeq3(): Seq[A] = {
    Seq(x, y, z)
  }

  def toSeq4(): Seq[A] = {
    Seq(x, y, z, w)
  }

  def toVec3():vec3[A] = {
    vec3(x,y,z)
  }

  @inline def xyz(): vec3[A] = vec3(x,y,z)


}

object vec4 {
  def apply[A : FieldLike](x: A, y: A, z: A, w: A)  = new vec4(x,y,z,w)
  def apply[A : FieldLike](v:vec3[A], w:A): vec4[A] = new vec4(v.x, v.y, v.z, w)
}


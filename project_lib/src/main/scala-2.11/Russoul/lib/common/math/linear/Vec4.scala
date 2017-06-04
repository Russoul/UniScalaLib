package Russoul.lib.common.math.linear

import Russoul.lib.common.immutable
import Russoul.lib.common.TypeClasses.{Euclidean, Field}
import Russoul.lib.common.TypeClasses.Field.Implicits._
/**
  *
  *
  * immutable
  */
@immutable case class Vec4[@specialized A](array:Array[A])(implicit ev: Field[A] with Euclidean[A]) {

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


  @inline def *(vec: Vec4[A]): A = {
    this (1) * vec(1) + this (2) * vec(2) + this (3) * vec(3) + this (4) * vec(4)
  }

  @inline def dotProduct(vec: Vec4[A]): A = {
    *(vec)
  }

  @inline def matrixProduct(matrix: Mat4[A]): Vec4[A] = {
    this * matrix
  }

  @inline def *(matrix: Mat4[A]): Vec4[A] = {
    Vec4(this * matrix.column(1), this * matrix.column(2), this * matrix.column(3), this * matrix.column(4))
  }

  @inline def *(scalar: A): Vec4[A] = {
    Vec4(this (1) * scalar, this (2) * scalar, this (3) * scalar, this (4) * scalar)
  }

  @inline def **(vec: Vec4[A]): Vec4[A] = {
    ⊗(vec)
  }

  @inline def ⊗(vec: Vec4[A]): Vec4[A] = {
    Vec4(this (1) * vec(1), this (2) * vec(2), this (3) * vec(3), this (4) * vec(4))
  }

  @inline def /(scalar: A): Vec4[A] = {
    Vec4(this (1) / scalar, this (2) / scalar, this (3) / scalar, this (4) / scalar)
  }

  @inline def scalarMultiplication(scalar: A): Vec4[A] = {
    *(scalar)
  }

  @inline def add(vec: Vec4[A]): Vec4[A] = {
    this + vec
  }

  @inline def subtract(vec: Vec4[A]): Vec4[A] = {
    this - vec
  }

  @inline def -(vec: Vec4[A]): Vec4[A] = {
    Vec4(this (1) - vec(1), this (2) - vec(2), this (3) - vec(3), this (4) - vec(4))
  }

  @inline def +(vec: Vec4[A]): Vec4[A] = {
    Vec4(this (1) + vec(1), this (2) + vec(2), this (3) + vec(3), this (4) + vec(4))
  }

  @inline def unary_-(): Vec4[A] = {
    this * (-ev.one)
  }

  def toMatrix4FirstColumn(): Mat4[A] = {

    Mat4(this (1), ev.zero, ev.zero, ev.zero,
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


  @inline def normalize(): Vec4[A] = {
    this * (ev.one / length())
  }

  @inline def wOne(): Vec4[A] = Vec4(this (1), this (2), this (3), ev.one)

  @inline def wZero(): Vec4[A] = Vec4(this (1), this (2), this (3), ev.zero)


  @inline def divideByW(): Vec4[A] = this * (ev.one / w)


  def toSeq2(): Seq[A] = {
    Seq(x, y)
  }

  def toSeq3(): Seq[A] = {
    Seq(x, y, z)
  }

  def toSeq4(): Seq[A] = {
    Seq(x, y, z, w)
  }

  def toVec3():Vec3[A] = {
    Vec3(x,y,z)
  }

  @inline def xyz(): Vec3[A] = Vec3(x,y,z)


}

object Vec4 {
  @inline def apply[@specialized A : Field](x: A, y: A, z: A, w: A)  = new Vec4(x,y,z,w)
  @inline def apply[@specialized A : Field](v:Vec3[A], w:A): Vec4[A] = new Vec4(v.x, v.y, v.z, w)
}


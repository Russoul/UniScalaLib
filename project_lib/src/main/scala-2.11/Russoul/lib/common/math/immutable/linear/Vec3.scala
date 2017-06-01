package Russoul.lib.common.math.immutable.linear

import java.nio.{ByteBuffer, FloatBuffer}

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.{Field, Euclidean}
import Russoul.lib.common.math.TypeClasses.Field.Implicits._

import scala.language.implicitConversions


/**
  *
  * immutable
  */



@immutable case class Vec3[A](array:Array[A])(implicit ev: Field[A], pow:Euclidean[A]) {

  @inline def x: A = array(0)

  @inline def y: A = array(1)

  @inline def z: A = array(2)


  private def this(dx:A,dy:A,dz:A){
    this(Array[A](dx,dy,dz))
  }

  def <(right:Vec3[A]):Boolean = { //TODO probably not needed
    if (ev.lt(x , right.x))
      return true
    else if (ev.gt(x , right.x))
      return false

    if (ev.lt(y , right.y))
      return true
    else if (ev.gt(y , right.y))
      return false

    if (ev.lt(z , right.z))
      return true
    else if (ev.gt(z , right.z))
      return false

    false
  }

  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): A = {
    array(index-1)
  }


  @inline def *(vec: Vec3[A]): A = {
    this (1) * vec(1) + this (2) * vec(2) + this (3) * vec(3)
  }

  @inline def *(mat:mat4[A]): Vec3[A] = {
    (Vec4(this,ev.one) * mat).toVec3()
  }

  //by element product
  @inline def **(vec:Vec3[A]):Vec3[A] = {
    ⊗(vec)
  }

  //by element product
  @inline def ⊗(vec:Vec3[A]):Vec3[A] = {
    Vec3(this.x*vec.x, this.y*vec.y, this.z*vec.z)
  }

  @inline def dotProduct(vec: Vec3[A]): A = {
    this * vec
  }


  @inline def *(scalar: A): Vec3[A] = {
    Vec3(x * scalar, y * scalar, z * scalar)
  }
  @inline def /(scalar:A):Vec3[A] = {
    Vec3(x / scalar, y / scalar, z / scalar)
  }

  @inline def scalarMultiplication(scalar: A): Vec3[A] = {
    *(scalar)
  }

  @inline def add(vec: Vec3[A]): Vec3[A] = {
    this + vec
  }

  @inline def subtract(vec: Vec3[A]): Vec3[A] = {
    this - vec
  }

  @inline def -(vec: Vec3[A]): Vec3[A] = {
    Vec3(x - vec.x, y - vec.y, z - vec.z)
  }

  @inline def +(vec: Vec3[A]): Vec3[A] = {
    Vec3(x + vec.x, y + vec.y, z + vec.z)
  }

  @inline def unary_-(): Vec3[A] = {
    this * (-ev.one)
  }

  @inline def ^(v:Vec3[A]): Vec3[A] = {
    ⨯(v)
  }

  @inline def ⨯(v:Vec3[A]): Vec3[A] = {
    Vec3(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  }

  @inline def crossProduct(v: Vec3[A]): Vec3[A] = {
    ⨯(v)
  }


  override def toString(): String = {
    "vec3( " + x + "; " + y + "; " + z + " )"
  }


  @inline def length(): A = {
    val r = x * x + y * y + z * z
    pow.sqrt(r)
  }

  @inline def squareLength(): A = {
    x * x + y * y + z * z
  }



  def toArray2f(): Array[A] = {
    Array(this (1), this (2))
  }

  def toArray3f(): Array[A] = {
    Array(this (1), this (2), this (3))
  }



  @inline def normalize(): Vec3[A] = {
    this * (ev.one / length())
  }

  @inline def wOne(): Vec4[A] = Vec4(this (1), this (2), this (3), ev.one)

  @inline def wZero(): Vec4[A] = Vec4(this (1), this (2), this (3), ev.zero)

  @inline def xy(): Vec2[A] = Vec2(x,y)


  def toSeq2(): Seq[A] = {
    Seq(x, y)
  }

  def toSeq3(): Seq[A] = {
    Seq(x, y, z)
  }

}



object Vec3 {
  def apply[A : Field](x: A, y: A, z: A) = new Vec3(x,y,z)
  def apply[A : Field](v2:Vec2[A], z:A) = new Vec3(v2.x, v2.y, z)
}




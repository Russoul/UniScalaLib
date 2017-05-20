package Russoul.lib.common.math.immutable.linear

import java.nio.{ByteBuffer, FloatBuffer}

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._

import scala.language.implicitConversions


/**
  *
  * immutable
  */



@immutable case class vec3[A](array:Array[A])(implicit ev: FieldLike[A]) {

  @inline def x: A = array(0)

  @inline def y: A = array(1)

  @inline def z: A = array(2)


  private def this(dx:A,dy:A,dz:A){
    this(Array[A](dx,dy,dz))
  }

  def <(right:vec3[A]):Boolean = { //TODO probably not needed
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


  @inline def *(vec: vec3[A]): A = {
    dotProduct(vec)
  }

  @inline def *(mat:mat4[A]): vec3[A] = {
    (vec4(this,ev.one) * mat).toVec3()
  }

  //by element product
  @inline def **(vec:vec3[A]):vec3[A] = {
    vec3(this.x*vec.x, this.y*vec.y, this.z*vec.z)
  }

  @inline def dotProduct(vec: vec3[A]): A = {
    this (1) * vec(1) + this (2) * vec(2) + this (3) * vec(3)
  }


  @inline def *(scalar: A): vec3[A] = {
    vec3(x * scalar, y * scalar, z * scalar)
  }
  @inline def /(scalar:A):vec3[A] = {
    vec3(x / scalar, y / scalar, z / scalar)
  }

  @inline def scalarMultiplication(scalar: A): vec3[A] = {
    vec3(this (1) * scalar, this (2) * scalar, this (3) * scalar)
  }

  @inline def add(vec: vec3[A]): vec3[A] = {
    vec3(x + vec.x, y + vec.y, z + vec.z)
  }

  @inline def subtract(vec: vec3[A]): vec3[A] = {
    vec3(x - vec.x, y - vec.y, z - vec.z)
  }

  @inline def -(vec: vec3[A]): vec3[A] = {
    vec3(x - vec.x, y - vec.y, z - vec.z)
  }

  @inline def +(vec: vec3[A]): vec3[A] = {
    vec3(x + vec.x, y + vec.y, z + vec.z)
  }

  @inline def unary_-(): vec3[A] = {
    this * (-ev.one)
  }

  @inline def ^(vec:vec3[A]): vec3[A] = {
    this.crossProduct(vec)
  }


  override def toString(): String = {
    "vec3( " + x + "; " + y + "; " + z + " )"
  }


  @inline def length(): A = {
    val r = x * x + y * y + z * z
    ev.sqrt(r)
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



  @inline def normalize(): vec3[A] = {
    this * (ev.one / length())
  }

  @inline def wOne(): vec4[A] = vec4(this (1), this (2), this (3), ev.one)

  @inline def wZero(): vec4[A] = vec4(this (1), this (2), this (3), ev.zero)

  @inline def xy(): vec2[A] = vec2(x,y)

  @inline def crossProduct(v: vec3[A]): vec3[A] = {
    vec3(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  }

  def toSeq2(): Seq[A] = {
    Seq(x, y)
  }

  def toSeq3(): Seq[A] = {
    Seq(x, y, z)
  }

}



object vec3 {
  def apply[A : FieldLike](x: A, y: A, z: A) = new vec3(x,y,z)
  def apply[A : FieldLike](v2:vec2[A], z:A) = new vec3(v2.x, v2.y, z)
}




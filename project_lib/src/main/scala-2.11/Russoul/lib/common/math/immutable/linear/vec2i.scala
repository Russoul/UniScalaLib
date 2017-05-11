package Russoul.lib.common.math.immutable.linear

import Russoul.lib.common.lang.immutable

/**
  *
  * immutable
  */
@immutable case class vec2i(array:Array[Int])
{


  @inline def x: Int = array(0)

  @inline def y: Int = array(1)

  private def this(dx:Int,dy:Int){
    this(Array(dx,dy))
  }




  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): Int = {
    array(index-1)
  }


  @inline def *(vec: vec2i): Int = {
    dotProduct(vec)
  }

  @inline def dotProduct(vec: vec2i): Int = {
    this (1) * vec(1) + this (2) * vec(2)
  }


  @inline def *(scalar: Float): vec2i = {
    scalarMultiplication(scalar)
  }

  @inline def scalarMultiplication(scalar: Float): vec2i = {
    new vec2i((this (1) * scalar).toInt, (this (2) * scalar).toInt)
  }

  @inline def add(vec: vec2i): vec2i = {
    new vec2i(this (1) + vec(1), this (2) + vec(2))
  }

  @inline def subtract(vec: vec2i): vec2i = {
    add(-vec)
  }

  @inline def -(vec: vec2i): vec2i = {
    subtract(vec)
  }

  @inline def +(vec: vec2i): vec2i = {
    add(vec)
  }

  @inline def unary_-(): vec2i = {
    this * (-1)
  }



  override def toString(): String = {
    "vec2i( " + x + "; " + y + " )"
  }


  @inline def length(): Int = {
    val r = x * x + y * y
    math.sqrt(r).toInt
  }

  @inline def squareLength(): Int = {
    x * x + y * y
  }



  def toArray2f(): Array[Int] = {
    Array(this (1), this (2))
  }



  @inline def normalize(): vec2i = {
    this * (1 / length())
  }


  def toSeq2(): Seq[Int] = {
    Seq(x, y)
  }



  override def equals(obj: scala.Any): Boolean =
  {
    obj match {
      case v: vec2i =>
        return x == v.x && y == v.y
      case _ =>
    }
    false
  }


}

object vec2i {

  def apply(x:Int, y:Int): vec2i = new vec2i(Array(x,y))

}
package Russoul.lib.common.math.immutable.linear

/**
  *
  * immutable
  */
class vec2i(arrayIn:Array[Int])
{

  protected[lib] val array = arrayIn

  def x = array(0)

  def y = array(1)

  def this(dx:Int,dy:Int){
    this(Array(dx,dy))
  }


  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  def apply(index: Int): Int =
  {
    array(index-1)
  }


  def *(vec: vec2i) =
  {
    dotProduct(vec)
  }

  def dotProduct(vec: vec2i): Int =
  {
    this (1) * vec(1) + this (2) * vec(2)
  }


  def *(scalar: Float): vec2i =
  {
    scalarMultiplication(scalar)
  }

  def scalarMultiplication(scalar: Float): vec2i =
  {
    vec2i((this (1) * scalar).toInt, (this (2) * scalar).toInt)
  }

  def add(vec: vec2i): vec2i =
  {
    vec2i(this (1) + vec(1), this (2) + vec(2))
  }

  def subtract(vec: vec2i): vec2i =
  {
    add(-vec)
  }

  def -(vec: vec2i): vec2i =
  {
    subtract(vec)
  }

  def +(vec: vec2i): vec2i =
  {
    add(vec)
  }

  def unary_-(): vec2i =
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


  def length(): Int =
  {
    val r = x * x + y * y
    math.sqrt(r).toInt
  }

  def squareLength(): Int =
  {
    x * x + y * y
  }



  def toArray2f(): Array[Int] =
  {
    Array(this (1), this (2))
  }


  def copy(): vec2i =
  {
    vec2i(this (1), this (2))
  }

  def normalize(): vec2i =
  {
    this * (1 / length())
  }


  def toSeq2(): Seq[Int] =
  {
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

object vec2i
{

  def apply(x: Int, y: Int) = new vec2i(x,y)

  def newSpace() = new Array[Int](2)

  def getByteSize() = 8
}
package Russoul.lib.common.math

import Russoul.lib.common.math.immutable.linear.vec3

/**
  * Created by Russoul on 20.04.2016.
  */
object Math
{

  final val PI: Double = 3.14159265358979323846
  final val EPSILON:Double = 0.0000000000000001
  final val GOLDEN_RATIO:Double = 1.61803398875

  def power(value: Int, pow: Int): Int =
  {
    var c = pow
    var res = 1
    while (c != 0) {
      res *= value
      c -= 1
    }

    res
  }


  def roundTo2Power(sub:Float, pow:Int): Float =
  {
    val p1 = power(2,pow-1)

    val d = if(sub < -p1) -1 else 0

    ((sub.toInt + d + p1) >> pow)*p1*2
  }

  def roundTo2Power(sub:vec3, pow:Int): vec3 =
  {
    val p1 = power(2,pow-1)

    val d1 = if(sub.x < -p1) -1 else 0
    val d2 = if(sub.y < -p1) -1 else 0
    val d3 = if(sub.z < -p1) -1 else 0

    vec3(((sub.x.toInt + d1 + p1) >> pow)*p1*2,((sub.y.toInt + d2 + p1) >> pow)*p1*2 ,((sub.z.toInt + d3 + p1) >> pow)*p1*2)
  }

  /**
    * Given a number n, this method returns n if n is a power-of-two.
    *
    * Otherwise, it returns the smallest power-of-two larger than n.
    */
  def nextPowerOfTwo(n: Int): Int = {
    val x = java.lang.Integer.highestOneBit(n)
    if (x == n) n else x * 2
  }

  implicit class FloatInverse(x:Float)
  {
    def inv():Float = -x
  }

  implicit class FloatSquared(x:Float)
  {
    def squared() = x*x
  }

  /**
    *
    * @param isoLevel must be between two values
    * @param v1
    * @param v2
    * @param val1
    * @param val2
    * @return
    */
  def vertexInterpolation(isoLevel:Float, v1:vec3, v2:vec3, val1:Float, val2:Float):vec3 =
  {
    var min:Float = -1
    var max:Float = -1

    var minV:vec3 = null
    var maxV:vec3 = null

    if(val1 >= val2)
    {
      max = val1
      min = val2

      maxV = v1
      minV = v2
    }else
    {
      min = val1
      max = val2

      minV = v1
      maxV = v2
    }

    val dist = max - min
    val k = (isoLevel-min)/(max-min)


    minV + (maxV - minV) * k
  }
}

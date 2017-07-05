package Russoul.lib.common.math

import Russoul.lib.common._
import Russoul.lib.common.{Real, Real3}
import Implicits._

/**
  * Created by Russoul on 20.04.2016.
  */
object Math
{

  final val PI: Real = 3.14159265358979323846
  final val EPSILON:Real = 0.0000000000000001
  final val GOLDEN_RATIO:Real = 1.61803398875

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


  def roundTo2Power(sub:Real, pow:Int): Int =
  {
    val p1 = power(2,pow-1)

    val d = if(sub < -p1) -1 else 0

    ((sub.toInt + d + p1) >> pow)*p1*2
  }

  def roundTo2Power(sub:Real3, pow:Int): Real3 =
  {
    val p1 = power(2,pow-1)

    val d1 = if(sub.x < -p1) -1 else 0
    val d2 = if(sub.y < -p1) -1 else 0
    val d3 = if(sub.z < -p1) -1 else 0

    Real3(((sub.x.toInt + d1 + p1) >> pow)*p1*2,((sub.y.toInt + d2 + p1) >> pow)*p1*2 ,((sub.z.toInt + d3 + p1) >> pow)*p1*2)
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

  /*def nextMultipleOf(n:Real, multipleOf:Real): Real ={


    if(n == 0) return 0

    def rec(curMult:Real): Real =
    {
      if(n <= curMult) curMult
      else{
        rec(curMult + multipleOf)
      }
    }

    rec(multipleOf)
  }*/


  implicit class RealEditions(x:Real)
  {
    def sq():Real = x*x
    def squared(): Real = x*x
    def inv():Real = -x
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
  def vertexInterpolation(isoLevel:Real, v1:Real3, v2:Real3, val1:Real, val2:Real):Real3 =
  {

    var min:Real = -1D
    var max:Real = -1D

    var minV = nil[Real3]
    var maxV = nil[Real3]

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

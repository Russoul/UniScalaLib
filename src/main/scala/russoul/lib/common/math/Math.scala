package russoul.lib.common.math

import russoul.lib.common._
import Implicits._

import spire.algebra._
import spire.math._
import spire.implicits._

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


  def roundTo2Power(sub:Double, pow:Int): Int =
  {
    val p1 = power(2,pow-1)

    val d = if(sub < -p1) -1 else 0

    ((sub.toInt + d + p1) >> pow)*p1*2
  }

  def roundTo2Power(sub:Double3, pow:Int): Double3 =
  {
    val p1 = power(2,pow-1)

    val d1 = if(sub(0) < -p1) -1 else 0
    val d2 = if(sub(1) < -p1) -1 else 0
    val d3 = if(sub(2) < -p1) -1 else 0

    Double3(((sub(0).toInt + d1 + p1) >> pow)*p1*2,((sub(1).toInt + d2 + p1) >> pow)*p1*2 ,((sub(2).toInt + d3 + p1) >> pow)*p1*2)
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

  /*def nextMultipleOf(n:Double, multipleOf:Double): Double ={


    if(n == 0) return 0

    def rec(curMult:Double): Double =
    {
      if(n <= curMult) curMult
      else{
        rec(curMult + multipleOf)
      }
    }

    rec(multipleOf)
  }*/


  implicit class DoubleEditions(x:Double)
  {
    def sq():Double = x*x
    def squared(): Double = x*x
    def inv():Double = -x
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
  def vertexInterpolation(isoLevel:Double, v1:Double3, v2:Double3, val1:Double, val2:Double):Double3 =
  {

    var min:Double = -1D
    var max:Double = -1D

    var minV = nil[Double3]
    var maxV = nil[Double3]

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

  def vertexInterpolation(v1:Float2, v2:Float2, val1:Float, val2:Float):Float2 =
  {

    var min:Float = -1F
    var max:Float = -1F

    var minV = nil[Float2]
    var maxV = nil[Float2]

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
    val k = (-min)/(max-min)



    minV + (maxV - minV) * k
  }



}

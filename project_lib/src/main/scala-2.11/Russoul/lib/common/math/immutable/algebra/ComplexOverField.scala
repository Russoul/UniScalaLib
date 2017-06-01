package Russoul.lib.common.math.immutable.algebra

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.{Field, Euclidean}
import Russoul.lib.common.math.TypeClasses.Field.Implicits._
import Russoul.lib.common.utils.Arr

/**
  * Created by russoul on 20.05.17.
  */
@immutable case class ComplexOverField[A](real: A, imaginary:A)(implicit ev: Field[A], pow:Euclidean[A]) {

  @inline def +(that:ComplexOverField[A]): ComplexOverField[A] =
  {
    ComplexOverField(real + that.real, imaginary + that.imaginary)
  }

  @inline def -(that:ComplexOverField[A]): ComplexOverField[A] =
  {
    ComplexOverField(real - that.real, imaginary - that.imaginary)
  }

  @inline def unary_-(): ComplexOverField[A] =
  {
    ComplexOverField(-real, -imaginary)
  }

  @inline def *(that:ComplexOverField[A]): ComplexOverField[A] =
  {
    ComplexOverField(real * that.real - imaginary * that.imaginary, real * that.imaginary + imaginary * that.real)
  }

  @inline def *(scalar: A): ComplexOverField[A] =
  {
    ComplexOverField(real * scalar, imaginary * scalar)
  }

  @inline def /(scalar: A): ComplexOverField[A] =
  {
    ComplexOverField(real / scalar, imaginary / scalar)
  }

  @inline  def squaredLength() : A = {
    real * real + imaginary * imaginary
  }

  @inline def length(): A ={
    pow.sqrt(squaredLength())
  }

  @inline def conjugate() : ComplexOverField[A] = {
    ComplexOverField(real, -imaginary)
  }

  @inline def /(that : ComplexOverField[A]): ComplexOverField[A] = {
    this * that / squaredLength()
  }



}

object ComplexOverField{


  @inline def root(n:Int, num:ComplexOverField[Float]) : Arr[ComplexOverField[Float]] = {
    val res = new Arr[ComplexOverField[Float]](new Array[ComplexOverField[Float]](n), 0)


    val r = num.length()
    val trig = num / r

    val phi = math.acos(trig.real).toFloat

    val rRooted = math.pow(r, 1/n.toFloat).toFloat
    for(i <- 0 until n){
      val newPhi = (phi + 2*math.Pi * i)/n.toFloat
      res += ComplexOverField(math.cos(newPhi).toFloat * rRooted, math.sin(newPhi).toFloat * rRooted)
    }

    res
  }
}

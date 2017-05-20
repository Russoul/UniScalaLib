package Russoul.lib.common.math.immutable.algebra

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._
import Russoul.lib.common.utils.Vector

/**
  * Created by russoul on 20.05.17.
  */
@immutable case class Complex[A](real: A, imaginary:A)(implicit ev: FieldLike[A]) {

  @inline def +(that:Complex[A]): Complex[A] =
  {
    Complex(real + that.real, imaginary + that.imaginary)
  }

  @inline def -(that:Complex[A]): Complex[A] =
  {
    Complex(real - that.real, imaginary - that.imaginary)
  }

  @inline def *(that:Complex[A]): Complex[A] =
  {
    Complex(real * that.real - imaginary * that.imaginary, real * that.imaginary + imaginary * that.real)
  }

  @inline def *(scalar: A): Complex[A] =
  {
    Complex(real * scalar, imaginary * scalar)
  }

  @inline def /(scalar: A): Complex[A] =
  {
    Complex(real / scalar, imaginary / scalar)
  }

  @inline  def squaredLength() : A = {
    real * real + imaginary * imaginary
  }

  @inline def length(): A ={
    ev.sqrt(squaredLength())
  }

  @inline def conjugate() : Complex[A] = {
    Complex(real, -imaginary)
  }

  @inline def /(that : Complex[A]): Complex[A] = {
    this * that / squaredLength()
  }



}

object Complex{


  @inline def root(n:Int, num:Complex[Float]) : Vector[Complex[Float]] = {
    val res = new Vector[Complex[Float]](new Array[Complex[Float]](n), 0)


    val r = num.length()
    val trig = num / r

    val phi = math.acos(trig.real).toFloat

    val rRooted = math.pow(r, 1/n.toFloat).toFloat
    for(i <- 0 until n){
      val newPhi = (phi + 2*math.Pi * i)/n.toFloat
      res += Complex(math.cos(newPhi).toFloat * rRooted, math.sin(newPhi).toFloat * rRooted)
    }

    res
  }
}

package russoul.lib.common.math.algebra

import russoul.lib.common.Implicits._
import russoul.lib.common._
import russoul.lib.common.utils.Arr

import scala.math

import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 20.05.17.
  */
@immutable case class ComplexOver[@specialized(Float,Double,Int) A](real: A, imaginary:A) {

  @inline def +(that:ComplexOver[A])(implicit ev: Field[A]): ComplexOver[A] =
  {
    ComplexOver(real + that.real, imaginary + that.imaginary)
  }

  @inline def -(that:ComplexOver[A])(implicit ev: Field[A]): ComplexOver[A] =
  {
    ComplexOver(real - that.real, imaginary - that.imaginary)
  }

  @inline def unary_-()(implicit ev: Field[A]): ComplexOver[A] =
  {
    ComplexOver(-real, -imaginary)
  }

  @inline def *(that:ComplexOver[A])(implicit ev: Field[A]): ComplexOver[A] =
  {
    ComplexOver(real * that.real - imaginary * that.imaginary, real * that.imaginary + imaginary * that.real)
  }

  @inline def *(scalar: A)(implicit ev: Field[A]): ComplexOver[A] =
  {
    ComplexOver(real * scalar, imaginary * scalar)
  }

  @inline def /(scalar: A)(implicit ev: Field[A]): ComplexOver[A] =
  {
    ComplexOver(real / scalar, imaginary / scalar)
  }

  @inline  def squaredLength()(implicit ev: Field[A]) : A = {
    real * real + imaginary * imaginary
  }

  @inline def length()(implicit ev: Field[A], ev2 : NRoot[A]): A ={
    ev2.sqrt(squaredLength())
  }

  @inline def conjugate()(implicit ev: Field[A]) : ComplexOver[A] = {
    ComplexOver(real, -imaginary)
  }

  @inline def /(that : ComplexOver[A])(implicit ev: Field[A]): ComplexOver[A] = {
    this * that / squaredLength()
  }



}

object ComplexOver{


  @inline def root(n:Int, num:ComplexOver[Float]) : Arr[ComplexOver[Float]] = {

    val res = new Arr[ComplexOver[Float]](new Array[ComplexOver[Float]](n), 0)


    val r = num.length()
    val trig = num / r

    val phi = math.acos(trig.real).toFloat

    val rRooted = math.pow(r, 1/n).toFloat
    for(i <- 0 until n){
      val newPhi = (phi + 2*math.Pi * i)/n
      res += ComplexOver[Float](math.cos(newPhi).toFloat * rRooted, math.sin(newPhi).toFloat * rRooted)
    }

    res
  }
}

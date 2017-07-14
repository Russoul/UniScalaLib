package russoul.lib.common.math.algebra

import russoul.lib.common.TypeClasses.{Euclidean, Field}
import russoul.lib.common.Implicits._
import russoul.lib.common.{Real, immutable, tbsp}
import russoul.lib.common.utils.Arr

/**
  * Created by russoul on 20.05.17.
  */
@immutable case class ComplexOver[@tbsp A](real: A, imaginary:A)(implicit ev: Field[A] with Euclidean[A]) {

  @inline def +(that:ComplexOver[A]): ComplexOver[A] =
  {
    ComplexOver(real + that.real, imaginary + that.imaginary)
  }

  @inline def -(that:ComplexOver[A]): ComplexOver[A] =
  {
    ComplexOver(real - that.real, imaginary - that.imaginary)
  }

  @inline def unary_-(): ComplexOver[A] =
  {
    ComplexOver(-real, -imaginary)
  }

  @inline def *(that:ComplexOver[A]): ComplexOver[A] =
  {
    ComplexOver(real * that.real - imaginary * that.imaginary, real * that.imaginary + imaginary * that.real)
  }

  @inline def *(scalar: A): ComplexOver[A] =
  {
    ComplexOver(real * scalar, imaginary * scalar)
  }

  @inline def /(scalar: A): ComplexOver[A] =
  {
    ComplexOver(real / scalar, imaginary / scalar)
  }

  @inline  def squaredLength() : A = {
    real * real + imaginary * imaginary
  }

  @inline def length(): A ={
    ev.sqrt(squaredLength())
  }

  @inline def conjugate() : ComplexOver[A] = {
    ComplexOver(real, -imaginary)
  }

  @inline def /(that : ComplexOver[A]): ComplexOver[A] = {
    this * that / squaredLength()
  }



}

object ComplexOver{


  @inline def root(n:Int, num:ComplexOver[Real]) : Arr[ComplexOver[Real]] = {
    import russoul.lib.common.TypeClasses.DoubleIsFullField._

    val res = new Arr[ComplexOver[Real]](new Array[ComplexOver[Real]](n), 0)


    val r = num.length()
    val trig = num / r

    val phi = math.acos(trig.real)

    val rRooted = math.pow(r, 1/n)
    for(i <- 0 until n){
      val newPhi = (phi + 2*math.Pi * i)/n
      res += ComplexOver(math.cos(newPhi) * rRooted, math.sin(newPhi) * rRooted)
    }

    res
  }
}

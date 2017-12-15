package russoul.lib.common

import russoul.lib.common.math.algebra._

import scala.language.{higherKinds, implicitConversions}
import shapeless.ops.nat.{Diff, GT, Sum, ToInt}
import shapeless.{<:!<, Nat, Succ}
import spire.algebra.{Field, Trig}
import spire.algebra._
import spire.math._
import spire.implicits._

import scala.reflect.ClassTag // provides infix operators, instances and conversions
/**
  * Created by russoul on 18.05.17.
  */
object TypeClasses {

  class OperationUnsupportedException(str: String) extends Exception(str)


  trait Container1[@tbsp T, Con]{
    def x(con: Con): T
  }
  trait Container2[@tbsp T, Con] extends Container1[T,Con]{
    def y(con: Con): T
  }
  trait Container3[@tbsp T, Con] extends Container2[T,Con]{
    def z(con: Con): T
  }
  trait Container4[@tbsp T, Con] extends Container3[T,Con]{
    def w(con: Con): T
  }

  trait ContainerAny[@tbsp T, Con] extends Container4[T,Con]{
    def apply(con: Con, i: Int) : T
    def size(con: Con) : Int
  }

  abstract class VecIsVectorSpace[@tbsp F : ClassTag : Field, Size <: Nat]() extends VectorSpace [Vec[F,Size],F]{
    override def scalar: Field[F] = Field[F]

    override def timesl(r: F, v: Vec[F, Size]): Vec[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = r * v(i)
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def negate(v: Vec[F, Size]): Vec[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = -v(i)
      }

      Vec[F, Size](ar : _*)
    }

    override def zero: Vec[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) =
      }

      Vec[F, Size](ar : _*)
    }

    override def plus(x: Vec[F, Size], y: Vec[F, Size]): Vec[F, Size] = ???
  }

  abstract class AlgVector[@tbsp F : ClassTag, Size <: Nat](){

    implicit def scalar: Field[F] with Trig[F] with NRoot[F]

    def x(a: Vec[F,Size])(implicit ev1: GT[Size, Nat._0]) = a(0)
    def y(a: Vec[F,Size])(implicit ev1: GT[Size, Nat._1]) = a(1)
    def z(a: Vec[F,Size])(implicit ev1: GT[Size, Nat._2]) = a(2)
    def w(a: Vec[F,Size])(implicit ev1: GT[Size, Nat._3]) = a(3)

    def plus(a: Vec[F,Size], b: Vec[F,Size]): Vec[F,Size] = {
      val seq = new Array[F](a.size)

      var i = 0
      while(i < a.size){
        seq(i) = a(i) + b(i)
        i += 1
      }

      Vec(seq : _*)
    }

    def zero(size : Int): Vec[F,Size]= {
      val seq = new Array[F](size)

      var i = 0
      while(i < size){
        seq(i) = scalar.zero
        i += 1
      }

      Vec(seq : _*)
    }

    def negate(a: Vec[F,Size]): Vec[F,Size] = {
      val seq = new Array[F](a.size)

      var i = 0
      while(i < a.size){
        seq(i) = -a(i)
        i += 1
      }

      Vec(seq : _*)
    }

    def times(a:Vec[F,Size], k:F): Vec[F,Size] = {
      val seq = new Array[F](a.size)

      var i = 0
      while(i < a.size){
        seq(i) = a(i) * k
        i += 1
      }

      Vec(seq : _*)
    }

    def elem(a: Vec[F,Size], b: Vec[F,Size]) : Vec[F,Size] = {
      val seq = new Array[F](a.size)

      var k = 0
      while(k < a.size){
        seq(k) = a(k) * b(k)
        k += 1
      }

      Vec(seq : _*)
    }

    def div(a:Vec[F,Size], k:F):Vec[F,Size] = times(a, 1 / k)

    def dotProduct(a: Vec[F,Size], b: Vec[F,Size]): F = {
      var res = scalar.zero

      var i = 0
      while(i < a.size){
        res += a(i) * b(i)
        i += 1
      }

      res
    }

    def squaredLength(a: Vec[F,Size]): F = dotProduct(a, a)

    def length(a: Vec[F,Size]): F = dotProduct(a, a).sqrt()

    def angle(a: Vec[F,Size], b: Vec[F,Size]): F = scalar.acos(scalar.div(scalar.div(dotProduct(a, b), length(a)), length(b)))


    def normalize(v: Vec[F,Size]): Vec[F,Size] = {
      val len = length(v)
      div(v, len)
    }


  }



  //TODO DEFINITIONS------------------------------------------------------



  //Those are just normal containers with dynamic sizes (not known at compile time, moreover size cannot change across instances of collection even at compile time)
  class Tuple2IsContainer2[@tbsp T] extends Container2[T, (T,T)]{
    override def x(v: (T,T)): T = v._1

    override def y(v: (T,T)): T = v._2
  }
  class Tuple3IsContainer3[@tbsp T] extends Container3[T, (T,T,T)]{
    override def x(v: (T,T,T)): T = v._1

    override def y(v: (T,T,T)): T = v._2

    override def z(v: (T,T,T)): T = v._3
  }
  class Tuple4IsContainer4[@tbsp T] extends Container4[T, (T,T,T,T)]{
    override def x(v: (T,T,T,T)): T = v._1

    override def y(v: (T,T,T,T)): T = v._2

    override def z(v: (T,T,T,T)): T = v._3

    override def w(v: (T,T,T,T)): T = v._4
  }
  class Vec2IsContainer2[@tbsp T] extends Container2[T, Vec2[T]]{
    override def x(v: Vec2[T]): T = v(0)

    override def y(v: Vec2[T]): T = v(1)
  }
  class Vec3IsContainer3[@tbsp T] extends Container3[T, Vec3[T]]{
    override def x(v: Vec3[T]): T = v(0)

    override def y(v: Vec3[T]): T = v(1)

    override def z(v: Vec3[T]): T = v(2)
  }
  class Vec4IsContainer4[@tbsp T] extends Container4[T, Vec4[T]]{
    override def x(v: Vec4[T]): T = v(0)

    override def y(v: Vec4[T]): T = v(1)

    override def z(v: Vec4[T]): T = v(2)

    override def w(v: Vec4[T]): T = v(3)
  }
  class ArrayIsContainerAny[@tbsp T] extends ContainerAny[T, Array[T]]{
    override def x(v: Array[T]): T = v(0)

    override def y(v: Array[T]): T = v(1)

    override def z(v: Array[T]): T = v(2)

    override def w(v: Array[T]): T = v(3)

    override def apply(con: Array[T], i: Int): T = con(i)

    override def size(con: Array[T]): Int = con.size
  }


}

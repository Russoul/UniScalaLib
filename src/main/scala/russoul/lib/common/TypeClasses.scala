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

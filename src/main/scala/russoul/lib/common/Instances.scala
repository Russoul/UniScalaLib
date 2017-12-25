package russoul.lib.common

import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra.{Mat, Row}
import shapeless.Nat
import singleton.ops.XInt


import scala.collection.mutable
import scala.reflect.ClassTag

import spire._
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 13.06.2017.
  */
object Instances {


  object VecSupport{
    //TODO
  }


  object AllInstances extends AllInstances

  final class VecIsModule[@specialized(Float, Double, Int, Long) F : ClassTag : Ring, Size <: XInt](implicit _size : ValueOf[Size]) extends Module [Row[F,Size],F] {
    override def scalar: Ring[F] = Ring[F]

    override def timesl(r: F, v: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = r * v(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def negate(v: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = -v(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def zero: Row[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.size){
        ar(i) = scalar.zero
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def plus(a: Row[F, Size], b: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }
  }

  final class VecIsVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends VectorSpace [Row[F,Size],F]{
    override def scalar: Field[F] = Field[F]

    override def timesl(r: F, v: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = r * v(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def negate(v: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = -v(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def zero: Row[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.size){
        ar(i) = scalar.zero
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def plus(a: Row[F, Size], b: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }
  }


  final class VecIsNormedVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size], nroot : NRoot[F]) extends NormedVectorSpace [Row[F,Size],F]{
    override def scalar: Field[F] = Field[F]

    override def timesl(r: F, v: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.length){
        ar(i) = r * v(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def negate(v: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.length){
        ar(i) = -v(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def zero: Row[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.length){
        ar(i) = scalar.zero
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def plus(a: Row[F, Size], b: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def norm(v: Row[F, Size]): F = {
      var ret = scalar.zero
      var i = 0
      while (i < v.size){
        ret += v(i) * v(i)
        i += 1
      }

      ret.sqrt()
    }
  }


  final class VecIsInnerProductSpace[@specialized(Double, Float) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends InnerProductSpace [Row[F,Size],F]{
    override def scalar: Field[F] = Field[F]

    override def timesl(r: F, v: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.length){
        ar(i) = r * v(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def negate(v: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.length){
        ar(i) = -v(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def zero: Row[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.length){
        ar(i) = scalar.zero
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def plus(a: Row[F, Size], b: Row[F, Size]): Row[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Row[F, Size](ar : _*)
    }

    override def dot(a: Row[F, Size], b: Row[F, Size]): F = {
      var i = 0
      var r = scalar.zero
      while (i < a.size){
        r += a(i) * b(i)
        i += 1
      }

      r
    }


  }

  trait VecInstanceM1 {
    type NI0[F,S <: XInt] = spire.NoImplicit[VectorSpace[Row[F,S], F]]

    implicit def vecModule[@sp(Int,Long,Float,Double) F: ClassTag: Ring, Size <: XInt : ValueOf](implicit no : NI0[F,Size]): VecIsModule[F, Size] =
      new VecIsModule[F,Size]
  }

  trait VecInstance0 extends VecInstanceM1{
    type NI1[F,S <: XInt] = spire.NoImplicit[NormedVectorSpace[Row[F,S], F]]

    implicit def vecIsVectorSpace[@sp(Float, Double) F : ClassTag : Field, Size <: XInt : ValueOf](implicit no : NI1[F,Size]) : VecIsVectorSpace[F,Size] = {
      new VecIsVectorSpace[F, Size]()
    }
  }

  trait VecInstance1 extends VecInstance0{
    implicit def vecIsInnerProductSpace[@sp(Float, Double) F : ClassTag : Field, Size <: XInt : ValueOf] : VecIsInnerProductSpace[F,Size] = {
      new VecIsInnerProductSpace[F, Size]()
    }
  }

  trait VecInstance2 extends VecInstance1{
    implicit def vecIsNormedVectorSpace[@sp(Float, Double) F : ClassTag : Field : NRoot, Size <: XInt : ValueOf] : VecIsNormedVectorSpace[F,Size] = {
      new VecIsNormedVectorSpace[F, Size]()
    }
  }

  trait VecInstances extends VecInstance2

  object VecInstances extends VecInstances



  trait AllInstances extends VecInstances

  

}

package russoul.lib.common

import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra.{Mat, Vec}
import shapeless.Nat
import shapeless.ops.nat.ToInt
import Nat._
import singleton.ops.XInt
import spire.algebra._
import spire.{NoImplicit, sp}

import scala.collection.mutable
import scala.reflect.ClassTag


import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 13.06.2017.
  */
object Instances {




  object AllInstances extends AllInstances

  final class VecIsModule[@specialized(Float, Double, Int, Long) F : ClassTag : Ring, Size <: XInt](implicit _size : ValueOf[Size]) extends Module [Vec[F,Size],F] {
    override def scalar: Ring[F] = Ring[F]

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
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def zero: Vec[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.size){
        ar(i) = scalar.zero
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def plus(a: Vec[F, Size], b: Vec[F, Size]): Vec[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Vec[F, Size](ar : _*)
    }
  }

  final class VecIsVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends VectorSpace [Vec[F,Size],F]{
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
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def zero: Vec[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.size){
        ar(i) = scalar.zero
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def plus(a: Vec[F, Size], b: Vec[F, Size]): Vec[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Vec[F, Size](ar : _*)
    }
  }


  final class VecIsNormedVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size], nroot : NRoot[F]) extends NormedVectorSpace [Vec[F,Size],F]{
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
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def zero: Vec[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.size){
        ar(i) = scalar.zero
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def plus(a: Vec[F, Size], b: Vec[F, Size]): Vec[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def norm(v: Vec[F, Size]): F = {
      var ret = scalar.zero
      var i = 0
      while (i < v.size){
        ret += v(i)
        i += 1
      }

      ret.sqrt()
    }
  }


  final class VecIsInnerProductSpace[@specialized(Double, Float) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends InnerProductSpace [Vec[F,Size],F]{
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
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def zero: Vec[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.size){
        ar(i) = scalar.zero
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def plus(a: Vec[F, Size], b: Vec[F, Size]): Vec[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Vec[F, Size](ar : _*)
    }

    override def dot(a: Vec[F, Size], b: Vec[F, Size]): F = {
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
    type NI0[F,S <: XInt] = NoImplicit[VectorSpace[Vec[F,S], F]]

    implicit def vecModule[@sp(Int,Long,Float,Double) F: ClassTag: Ring, Size <: XInt : ValueOf](implicit no : NI0[F,Size]): VecIsModule[F, Size] =
      new VecIsModule[F,Size]
  }

  trait VecInstance0 extends VecInstanceM1{
    type NI1[F,S <: XInt] = NoImplicit[NormedVectorSpace[Vec[F,S], F]]

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

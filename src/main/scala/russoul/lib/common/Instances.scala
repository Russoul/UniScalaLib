package russoul.lib.common

import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra.{Column, Mat, Row}
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

  final class RowVecIsModule[@specialized(Float, Double, Int, Long) F : ClassTag : Ring, Size <: XInt](implicit _size : ValueOf[Size]) extends Module [Row[F,Size],F] {
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

  final class RowVecIsVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends VectorSpace [Row[F,Size],F]{
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


  final class RowVecIsNormedVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size], nroot : NRoot[F]) extends NormedVectorSpace [Row[F,Size],F]{
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


  final class RowVecIsInnerProductSpace[@specialized(Double, Float) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends InnerProductSpace [Row[F,Size],F]{
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


  //==========================================================================================

  final class ColumnVecIsModule[@specialized(Float, Double, Int, Long) F : ClassTag : Ring, Size <: XInt](implicit _size : ValueOf[Size]) extends Module [Column[F,Size],F] {
    override def scalar: Ring[F] = Ring[F]

    override def timesl(r: F, v: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = r * v(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def negate(v: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = -v(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def zero: Column[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.size){
        ar(i) = scalar.zero
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def plus(a: Column[F, Size], b: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }
  }

  final class ColumnVecIsVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends VectorSpace [Column[F,Size],F]{
    override def scalar: Field[F] = Field[F]

    override def timesl(r: F, v: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = r * v(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def negate(v: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.size){
        ar(i) = -v(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def zero: Column[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.size){
        ar(i) = scalar.zero
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def plus(a: Column[F, Size], b: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }
  }


  final class ColumnVecIsNormedVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size], nroot : NRoot[F]) extends NormedVectorSpace [Column[F,Size],F]{
    override def scalar: Field[F] = Field[F]

    override def timesl(r: F, v: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.length){
        ar(i) = r * v(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def negate(v: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.length){
        ar(i) = -v(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def zero: Column[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.length){
        ar(i) = scalar.zero
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def plus(a: Column[F, Size], b: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def norm(v: Column[F, Size]): F = {
      var ret = scalar.zero
      var i = 0
      while (i < v.size){
        ret += v(i) * v(i)
        i += 1
      }

      ret.sqrt()
    }
  }


  final class ColumnVecIsInnerProductSpace[@specialized(Double, Float) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends InnerProductSpace [Column[F,Size],F]{
    override def scalar: Field[F] = Field[F]

    override def timesl(r: F, v: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.length){
        ar(i) = r * v(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def negate(v: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](v.size)

      var i = 0
      while (i < ar.length){
        ar(i) = -v(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def zero: Column[F, Size] = {
      val ar = new Array[F](_size.value)

      var i = 0
      while (i < ar.length){
        ar(i) = scalar.zero
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def plus(a: Column[F, Size], b: Column[F, Size]): Column[F, Size] = {
      val ar = new Array[F](a.size)

      var i = 0
      while (i < a.size){
        ar(i) = a(i) + b(i)
        i += 1
      }

      Column[F, Size](ar : _*)
    }

    override def dot(a: Column[F, Size], b: Column[F, Size]): F = {
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

    implicit def rowVecModule[@sp(Int,Long,Float,Double) F: ClassTag: Ring, Size <: XInt : ValueOf](implicit no : NI0[F,Size]): RowVecIsModule[F, Size] =
      new RowVecIsModule[F,Size]

    implicit def columnVecModule[@sp(Int,Long,Float,Double) F: ClassTag: Ring, Size <: XInt : ValueOf](implicit no : NI0[F,Size]): ColumnVecIsModule[F, Size] =
      new ColumnVecIsModule[F,Size]
  }

  trait VecInstance0 extends VecInstanceM1{
    type NI1[F,S <: XInt] = spire.NoImplicit[NormedVectorSpace[Row[F,S], F]]

    implicit def rowVecIsVectorSpace[@sp(Float, Double) F : ClassTag : Field, Size <: XInt : ValueOf](implicit no : NI1[F,Size]) : RowVecIsVectorSpace[F,Size] = {
      new RowVecIsVectorSpace[F, Size]()
    }

    implicit def columnIsVectorSpace[@sp(Float, Double) F : ClassTag : Field, Size <: XInt : ValueOf](implicit no : NI1[F,Size]) : ColumnVecIsVectorSpace[F,Size] = {
      new ColumnVecIsVectorSpace[F, Size]()
    }
  }

  trait VecInstance1 extends VecInstance0{
    implicit def rowVecIsInnerProductSpace[@sp(Float, Double) F : ClassTag : Field, Size <: XInt : ValueOf] : RowVecIsInnerProductSpace[F,Size] = {
      new RowVecIsInnerProductSpace[F, Size]()
    }

    implicit def columnVecIsInnerProductSpace[@sp(Float, Double) F : ClassTag : Field, Size <: XInt : ValueOf] : ColumnVecIsInnerProductSpace[F,Size] = {
      new ColumnVecIsInnerProductSpace[F, Size]()
    }
  }

  trait VecInstance2 extends VecInstance1{
    implicit def rowVecIsNormedVectorSpace[@sp(Float, Double) F : ClassTag : Field : NRoot, Size <: XInt : ValueOf] : RowVecIsNormedVectorSpace[F,Size] = {
      new RowVecIsNormedVectorSpace[F, Size]()
    }

    implicit def columnVecIsNormedVectorSpace[@sp(Float, Double) F : ClassTag : Field : NRoot, Size <: XInt : ValueOf] : ColumnVecIsNormedVectorSpace[F,Size] = {
      new ColumnVecIsNormedVectorSpace[F, Size]()
    }
  }

  trait VecInstances extends VecInstance2

  object VecInstances extends VecInstances



  trait AllInstances extends VecInstances

  

}

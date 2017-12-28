package russoul.lib.common

import scala.language.implicitConversions
import scala.language.experimental.macros
import russoul.lib.common.TypeClasses._
import algebra.ring.AdditiveGroup
import russoul.lib.common.math.algebra.{Column, Mat, Row}

import scala.reflect.ClassTag
import russoul.lib.macros.Enricher
import singleton.ops.XInt
import Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._
import machinist.DefaultOps

/**
  * Created by russoul on 13.06.2017.
  */
object Ops {




  class Container1Ops[@specialized(Float,Double,Int) T, Con](lhs: Con)(implicit ev: Container1[T,Con]){
    @inline def x: T = ev.x(lhs)
  }
  class Container2Ops[@specialized(Float,Double,Int) T, Con2](lhs: Con2)(implicit ev: Container2[T,Con2]){
    //@inline def x: T = ev.x(lhs)
    @inline def y: T = ev.y(lhs)
  }
  class Container3Ops[@specialized(Float,Double,Int) T, Con3](lhs: Con3)(implicit ev: Container3[T,Con3]){
    //@inline def x: T = ev.x(lhs)
    //@inline def y: T = ev.y(lhs)
    @inline def z: T = ev.z(lhs)
  }
  class Container4Ops[@specialized(Float,Double,Int) T, Con4](lhs: Con4)(implicit ev: Container4[T,Con4]){
    //@inline def x: T = ev.x(lhs)
    //@inline def y: T = ev.y(lhs)
    //@inline def z: T = ev.z(lhs)
    @inline def w: T = ev.w(lhs)
  }
  class ContainerAnyOps[@specialized(Float,Double,Int) T, Con](lhs: Con)(implicit ev: ContainerAny[T,Con]){
    /*@inline def x: T = ev.x(lhs)
    @inline def y: T = ev.y(lhs)
    @inline def z: T = ev.z(lhs)
    @inline def w: T = ev.w(lhs)*/
    @inline def apply(i: Int): T = ev.apply(lhs,i)
    @inline def size(): Int = ev.size(lhs)
  }


  class RowVecOps[@specialized(Float,Double,Int) A : ClassTag, Size <: XInt : ValueOf](a : Row[A, Size]){

    def *(rhs:A)(implicit ev: VectorSpace[Row[A,Size], A]): Row[A,Size] = ev.timesr(a, rhs)

    def product(rhs : Row[A,Size])(implicit ev : Ring[A]) : Row[A,Size] = {
      val ar = new Array[A](a.size())

      var i = 0
      while(i < ar.length){
        ar(i) = a(i) * rhs(i)
        i += 1
      }

      Row[A, Size](ar : _*)
    }

    def ⊗(rhs : Row[A,Size])(implicit ev : Ring[A]) : Row[A,Size] = {
      val ar = new Array[A](a.size())

      var i = 0
      while(i < ar.length){
        ar(i) = a(i) * rhs(i)
        i += 1
      }

      Row[A, Size](ar : _*)
    }


    /*def *[M <: XInt : ValueOf](b : Mat[A, Size, M])(implicit field : Field[A], nroot : NRoot[A]) : Row[A, M] = { // 1xSize * SizexN = 1xM
      val ar = new Array[A](b.m)


      var i = 0
      while(i < ar.length){
        ar(i) = a dot b.column(i)
        i += 1
      }

      Row[A, M](ar : _*)
    }*/

    def squaredLength()(implicit ring : Ring[A]): A ={
      var ret = ring.zero
      var i = 0
      while(i < a.size){
        ret += a(i) * a(i)
        i += 1
      }

      ret
    }
  }

  class ColumnVecOps[@specialized(Float,Double,Int) A : ClassTag, Size <: XInt : ValueOf](a : Column[A, Size]){

    def *(rhs:A)(implicit ev: VectorSpace[Column[A,Size], A]): Column[A,Size] = ev.timesr(a, rhs)

    def product(rhs : Column[A,Size])(implicit ev : Ring[A]) : Column[A,Size] = {
      val ar = new Array[A](a.size())

      var i = 0
      while(i < ar.length){
        ar(i) = a(i) * rhs(i)
        i += 1
      }

      Column[A, Size](ar : _*)
    }

    def ⊗(rhs : Column[A,Size])(implicit ev : Ring[A]) : Column[A,Size] = {
      val ar = new Array[A](a.size())

      var i = 0
      while(i < ar.length){
        ar(i) = a(i) * rhs(i)
        i += 1
      }

      Column[A, Size](ar : _*)
    }


    def squaredLength()(implicit ring : Ring[A]): A ={
      var ret = ring.zero
      var i = 0
      while(i < a.size){
        ret += a(i) * a(i)
        i += 1
      }

      ret
    }
  }

  class ColumnVec4Ops[@specialized(Float,Double,Int) A : ClassTag](a : Column[A, _4]){

    def x = a(0) //TODO make faster
    def y = a(1)
    def z = a(2)
    def w = a(3)
  }

  class ColumnVec3Ops[@specialized(Float,Double,Int) A : ClassTag](a : Column[A, _3]){
    def ⨯(b : Column[A, _3])(implicit ring : Ring[A]) : Column[A,_3] = {
      Column[A,_3](a(1) * b(2) - b(1) * a(2), -(a(0)*b(2) - b(0)*a(2)), a(0) * b(1) - b(0) * a(1))
    }

    def cross(b : Column[A, _3])(implicit ring : Ring[A]) : Column[A,_3] = {
      Column[A,_3](a(1) * b(2) - b(1) * a(2), -(a(0)*b(2) - b(0)*a(2)), a(0) * b(1) - b(0) * a(1))
    }

    def x = a(0) //TODO make faster
    def y = a(1)
    def z = a(2)
  }

  class ColumnVec2Ops[@specialized(Float,Double,Int) A : ClassTag](a : Column[A, _2]){
    def ⟂()(implicit ev : Ring[A]) : Column[A, _2] = {
      Column[A,_2](-a(1), a(0))
    }

    def ortho()(implicit ev : Ring[A]) : Column[A, _2] = {
      Column[A,_2](-a(1), a(0))
    }

    def x = a(0) //TODO make faster
    def y = a(1)
  }

  class RowVec4Ops[@specialized(Float,Double,Int) A : ClassTag](a : Row[A, _4]){

    def x = a(0) //TODO make faster
    def y = a(1)
    def z = a(2)
    def w = a(3)
  }

  class RowVec3Ops[@specialized(Float,Double,Int) A : ClassTag](a : Row[A, _3]){
    def ⨯(b : Row[A, _3])(implicit ring : Ring[A]) : Row[A,_3] = {
      Vec3[A](a(1) * b(2) - b(1) * a(2), -(a(0)*b(2) - b(0)*a(2)), a(0) * b(1) - b(0) * a(1))
    }

    def cross(b : Row[A, _3])(implicit ring : Ring[A]) : Row[A,_3] = {
      Vec3[A](a(1) * b(2) - b(1) * a(2), -(a(0)*b(2) - b(0)*a(2)), a(0) * b(1) - b(0) * a(1))
    }

    def x = a(0) //TODO make faster
    def y = a(1)
    def z = a(2)
  }

  class RowVec2Ops[@specialized(Float,Double,Int) A : ClassTag](a : Row[A, _2]){
    def ⟂()(implicit ev : Ring[A]) : Row[A, _2] = {
      Vec2[A](-a(1), a(0))
    }

    def ortho()(implicit ev : Ring[A]) : Row[A, _2] = {
      Vec2[A](-a(1), a(0))
    }

    def x = a(0) //TODO make faster
    def y = a(1)
  }

  class MatrixOps[@specialized(Float,Double,Int) A : ClassTag , A1 <: XInt : ValueOf, A2 <: XInt : ValueOf](lhs : Mat[A,A1,A2]){
    def +(rhs : Mat[A, A1, A2])(implicit ev : AdditiveGroup[A]): Mat[A, A1, A2] ={

      val n = lhs.size() / lhs.m
      val ar = new Array[A](lhs.size())

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(i) + rhs(i)
        i += 1
      }

      Mat[A, A1, A2](n, lhs.m, ar : _*)
    }

    def -(rhs : Mat[A, A1, A2])(implicit ev : AdditiveGroup[A]): Mat[A, A1, A2] ={

      val n = lhs.size() / lhs.m
      val ar = new Array[A](lhs.size())

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(i) - rhs(i)
        i += 1
      }

      Mat[A, A1, A2](n, lhs.m, ar : _*)
    }

    def *(rhs : A)(implicit ev : Field[A]): Mat[A, A1, A2] ={

      val n = lhs.size() / lhs.m
      val ar = new Array[A](lhs.size())

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(i) * rhs
        i += 1
      }

      Mat[A, A1, A2](n, lhs.m, ar : _*)
    }

    def row(index : Int) : Row[A, A2] = {
      val ar = new Array[A](lhs.m)

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(index, i)
        i += 1
      }

      Row[A, A2](ar : _*)
    }

    def rowAsColumn(index : Int) : Column[A, A2] = {
      val ar = new Array[A](lhs.m)

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(index, i)
        i += 1
      }

      Column[A, A2](ar : _*)
    }

    def column(index : Int) : Column[A, A1] = {
      val ar = new Array[A](lhs.m)

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(i, index)
        i += 1
      }

      Column[A, A1](ar : _*)
    }

    def columnAsRow(index : Int) : Row[A, A1] = {
      val ar = new Array[A](lhs.m)

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(i, index)
        i += 1
      }

      Row[A, A1](ar : _*)
    }

    def ⨯[A3 <: XInt : ValueOf](rhs : Mat[A, A2, A3])(implicit field : Field[A]) : Mat[A, A1, A3] = {
      val n = lhs.size() / lhs.m
      val m = rhs.m

      val ar = new Array[A](n * m)

      for(i <- 0 until n){
        for(j <- 0 until m){
          ar(i * m + j) = lhs.row(i) dot rhs.columnAsRow(j)
        }
      }

      Mat[A, A1, A3](n, m, ar : _*)
    }

    def trans() : Mat[A,A2,A1] = {
      val ar = new Array[A](lhs.size())

      val n = lhs.size() / lhs.m
      val m = lhs.m


      for(i <- 0 until n){
        for(j <- 0 until m){
          ar(i * m + j) = lhs(j,i)
        }
      }

      Mat[A, A2, A1](m, n, ar : _*)
    }


  }



  trait ContainerImplicits{
    implicit def Container1Ops[@specialized(Float,Double,Int) T, Con1](x: Con1)(implicit ev : Container1[T,Con1]): Container1Ops[T,Con1] = new Container1Ops[T,Con1](x)
    implicit def Container2Ops[@specialized(Float,Double,Int) T, Con2](x: Con2)(implicit ev : Container2[T,Con2]): Container2Ops[T,Con2] = new Container2Ops[T,Con2](x)
    implicit def Container3Ops[@specialized(Float,Double,Int) T, Con3](x: Con3)(implicit ev : Container3[T,Con3]): Container3Ops[T,Con3] = new Container3Ops[T,Con3](x)
    implicit def Container4Ops[@specialized(Float,Double,Int) T, Con4](x: Con4)(implicit ev : Container4[T,Con4]): Container4Ops[T,Con4] = new Container4Ops[T,Con4](x)
    implicit def ContainerAnyOps[@specialized(Float,Double,Int) T, Con](x: Con)(implicit ev : ContainerAny[T,Con]): ContainerAnyOps[T,Con] = new ContainerAnyOps[T,Con](x)
  }

  trait MatrixImplicits{
    implicit def matrixOps[@specialized(Float,Double,Int) A, A1 <: XInt : ValueOf, A2 <: XInt : ValueOf](lhs : Mat[A,A1,A2])(implicit tag : ClassTag[A]) : MatrixOps[A,A1,A2] = new MatrixOps(lhs)
  }

  trait VectorImplicits{
    implicit def rowVec4Ops[@specialized(Float,Double,Int) A : ClassTag](lhs : Vec4[A]) : RowVec4Ops[A] = new RowVec4Ops[A](lhs)
    implicit def rowVec3Ops[@specialized(Float,Double,Int) A : ClassTag](lhs : Vec3[A]) : RowVec3Ops[A] = new RowVec3Ops[A](lhs)
    implicit def rowVec2Ops[@specialized(Float,Double,Int) A : ClassTag](lhs : Vec2[A]) : RowVec2Ops[A] = new RowVec2Ops[A](lhs)
    implicit def rowVecOps[@specialized(Float,Double,Int) A : ClassTag, N <: XInt : ValueOf](lhs : Row[A, N]) : RowVecOps[A, N] = new RowVecOps[A, N](lhs)
  }



  object ContainerImplicits extends ContainerImplicits

  trait AllOps extends
  ContainerImplicits with
    MatrixImplicits with
    VectorImplicits

}

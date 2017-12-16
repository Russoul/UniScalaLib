package russoul.lib.common

import scala.language.implicitConversions
import scala.language.experimental.macros
import russoul.lib.common.TypeClasses._

import algebra.ring.AdditiveGroup
import russoul.lib.common.math.algebra.{Mat, Vec}

import scala.reflect.ClassTag
import russoul.lib.macros.Enricher
import singleton.ops.XInt

import Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 13.06.2017.
  */
object Ops {




  class Container1Ops[@tbsp T, Con](lhs: Con)(implicit ev: Container1[T,Con]){
    @inline def x: T = ev.x(lhs)
  }
  class Container2Ops[@tbsp T, Con2](lhs: Con2)(implicit ev: Container2[T,Con2]){
    //@inline def x: T = ev.x(lhs)
    @inline def y: T = ev.y(lhs)
  }
  class Container3Ops[@tbsp T, Con3](lhs: Con3)(implicit ev: Container3[T,Con3]){
    //@inline def x: T = ev.x(lhs)
    //@inline def y: T = ev.y(lhs)
    @inline def z: T = ev.z(lhs)
  }
  class Container4Ops[@tbsp T, Con4](lhs: Con4)(implicit ev: Container4[T,Con4]){
    //@inline def x: T = ev.x(lhs)
    //@inline def y: T = ev.y(lhs)
    //@inline def z: T = ev.z(lhs)
    @inline def w: T = ev.w(lhs)
  }
  class ContainerAnyOps[@tbsp T, Con](lhs: Con)(implicit ev: ContainerAny[T,Con]){
    /*@inline def x: T = ev.x(lhs)
    @inline def y: T = ev.y(lhs)
    @inline def z: T = ev.z(lhs)
    @inline def w: T = ev.w(lhs)*/
    @inline def apply(i: Int): T = ev.apply(lhs,i)
    @inline def size(): Int = ev.size(lhs)
  }


  class VecOps[@tbsp A : ClassTag, Size <: XInt](a : Vec[A, Size]){
    def *[M <: XInt](b : Mat[A, Size, M])(implicit field : Field[A]) : Vec[A, M] = { // 1xSize * SizexN = 1xM
      val ar = new Array[A](b.m)


      var i = 0
      while(i < ar.length){
        ar(i) = a dot b.column(i)
        i += 1
      }

      Vec[A, M](ar : _*)
    }
  }

  class Vec4Ops[@tbsp A : ClassTag](a : Vec[A, _4]){

    def x = a(0) //TODO make faster
    def y = a(1)
    def z = a(2)
    def w = a(3)
  }

  class Vec3Ops[@tbsp A : ClassTag](a : Vec[A, _3]){
    def ⨯(b : Vec[A, _3])(implicit ring : Field[A]) : Vec[A,_3] = {
      Vec3[A](a(1) * b(2) - b(1) * a(2), -(a(0)*b(2) - b(0)*a(2)), a(0) * b(1) - b(0) * a(1))
    }

    def cross(b : Vec[A, _3])(implicit ring : Field[A]) : Vec[A,_3] = {
      Vec3[A](a(1) * b(2) - b(1) * a(2), -(a(0)*b(2) - b(0)*a(2)), a(0) * b(1) - b(0) * a(1))
    }

    def x = a(0) //TODO make faster
    def y = a(1)
    def z = a(2)
  }

  class Vec2Ops[@tbsp A : ClassTag](a : Vec[A, _2]){
    def ⟂()(implicit ev : Ring[A]) : Vec[A, _2] = {
      Vec2[A](-a(1), a(0))
    }

    def ortho()(implicit ev : Ring[A]) : Vec[A, _2] = {
      Vec2[A](-a(1), a(0))
    }

    def x = a(0) //TODO make faster
    def y = a(1)
  }

  class MatrixOps[@tbsp A : ClassTag , A1 <: XInt, A2 <: XInt](lhs : Mat[A,A1,A2]){
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

    def row(index : Int) : Vec[A, A2] = {
      val ar = new Array[A](lhs.m)

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(index, i)
        i += 1
      }

      Vec[A, A2](ar : _*)
    }

    def column(index : Int) : Vec[A, A1] = {
      val ar = new Array[A](lhs.m)

      var i = 0
      while(i < ar.length){
        ar(i) = lhs(i, index)
        i += 1
      }

      Vec[A, A1](ar : _*)
    }

    def ⨯[A3 <: XInt](rhs : Mat[A, A2, A3])(implicit field : Field[A]) : Mat[A, A1, A3] = {
      val n = lhs.size() / lhs.m
      val m = rhs.m

      val ar = new Array[A](n * m)

      for(i <- 0 until n){
        for(j <- 0 until m){
          ar(i * m + j) = lhs.row(i) dot rhs.column(j)
        }
      }

      Mat[A, A1, A3](n, m, ar : _*)
    }


  }



  trait ContainerImplicits{
    implicit def Container1Ops[@tbsp T, Con1](x: Con1)(implicit ev : Container1[T,Con1]): Container1Ops[T,Con1] = new Container1Ops[T,Con1](x)
    implicit def Container2Ops[@tbsp T, Con2](x: Con2)(implicit ev : Container2[T,Con2]): Container2Ops[T,Con2] = new Container2Ops[T,Con2](x)
    implicit def Container3Ops[@tbsp T, Con3](x: Con3)(implicit ev : Container3[T,Con3]): Container3Ops[T,Con3] = new Container3Ops[T,Con3](x)
    implicit def Container4Ops[@tbsp T, Con4](x: Con4)(implicit ev : Container4[T,Con4]): Container4Ops[T,Con4] = new Container4Ops[T,Con4](x)
    implicit def ContainerAnyOps[@tbsp T, Con](x: Con)(implicit ev : ContainerAny[T,Con]): ContainerAnyOps[T,Con] = new ContainerAnyOps[T,Con](x)
  }

  trait MatrixImplicits{
    implicit def matrixOps[@tbsp A, A1 <: XInt, A2 <: XInt](lhs : Mat[A,A1,A2])(implicit tag : ClassTag[A]) : MatrixOps[A,A1,A2] = new MatrixOps(lhs)
  }

  trait VectorImplicits{
    implicit def vector4Ops[@tbsp A : ClassTag](lhs : Vec4[A]) : Vec4Ops[A] = new Vec4Ops[A](lhs)
    implicit def vector3Ops[@tbsp A : ClassTag](lhs : Vec3[A]) : Vec3Ops[A] = new Vec3Ops[A](lhs)
    implicit def vector2Ops[@tbsp A : ClassTag](lhs : Vec2[A]) : Vec2Ops[A] = new Vec2Ops[A](lhs)
    implicit def vectorOps[@tbsp A : ClassTag : Field, N <: XInt](lhs : Vec[A, N]) : VecOps[A, N] = new VecOps[A, N](lhs)
  }



  object ContainerImplicits extends ContainerImplicits

  trait AllOps extends
  ContainerImplicits with
    MatrixImplicits with
    VectorImplicits

}

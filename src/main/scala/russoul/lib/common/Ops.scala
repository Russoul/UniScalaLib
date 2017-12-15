package russoul.lib.common

import scala.language.implicitConversions
import scala.language.experimental.macros
import russoul.lib.common.TypeClasses._
import shapeless.Nat
import shapeless.ops.nat.{GT, LT, ToInt}
import Nat._
import Implicits._
import russoul.lib.common.math.algebra.Vec

import scala.reflect.ClassTag
import russoul.lib.macros.Enricher


/**
  * Created by russoul on 13.06.2017.
  */
object Ops {



  class VectorsOps[@tbsp F : ClassTag, Dim <: Nat](lhs: Vec[F,Dim])(implicit ev: AlgVector[F,Dim]){

    @inline def *(rhs: F) : Vec[F,Dim] = macro Enricher.binop[Vec[F,Dim], Vec[F,Dim]]
    @inline def /(rhs: F) : Vec[F,Dim] = macro Enricher.binop[Vec[F,Dim], Vec[F,Dim]]
    @inline def ⊗(rhs: Vec[F,Dim]) : Vec[F,Dim] = macro Enricher.binop[Vec[F,Dim], Vec[F,Dim]]
    @inline def elem(rhs: Vec[F,Dim]) : Vec[F,Dim] = macro Enricher.binop[Vec[F,Dim], Vec[F,Dim]]

    @inline def x(implicit ev1: GT[Dim, Nat._0]) = macro Enricher.unopWithEv2[GT[Dim, Nat._0], F]
    @inline def y(implicit ev1: GT[Dim, Nat._1]) = macro Enricher.unopWithEv2[GT[Dim, Nat._1], F]
    @inline def z(implicit ev1: GT[Dim, Nat._2]) = macro Enricher.unopWithEv2[GT[Dim, Nat._2], F]
    @inline def w(implicit ev1: GT[Dim, Nat._3]) = macro Enricher.unopWithEv2[GT[Dim, Nat._3], F]


    @inline def ⋅(rhs: Vec[F,Dim]) : F = ev.dotProduct(lhs, rhs)
    @inline def dot(rhs: Vec[F,Dim]) : F = ev.dotProduct(lhs, rhs)
    //can't use * operator because of JVM type erasure (we already have this operator in Field[F] both are erased to Object => Object)
    @inline def normalize() : Vec[F,Dim] = ev.normalize(lhs)
    @inline def squaredLength() : F = ev.squaredLength(lhs)
    @inline def length() : F = ev.scalar.sqrt(ev.squaredLength(lhs))
  }


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




  trait ContainerImplicits{
    implicit def Container1Ops[@tbsp T, Con1](x: Con1)(implicit ev : Container1[T,Con1]): Container1Ops[T,Con1] = new Container1Ops[T,Con1](x)
    implicit def Container2Ops[@tbsp T, Con2](x: Con2)(implicit ev : Container2[T,Con2]): Container2Ops[T,Con2] = new Container2Ops[T,Con2](x)
    implicit def Container3Ops[@tbsp T, Con3](x: Con3)(implicit ev : Container3[T,Con3]): Container3Ops[T,Con3] = new Container3Ops[T,Con3](x)
    implicit def Container4Ops[@tbsp T, Con4](x: Con4)(implicit ev : Container4[T,Con4]): Container4Ops[T,Con4] = new Container4Ops[T,Con4](x)
    implicit def ContainerAnyOps[@tbsp T, Con](x: Con)(implicit ev : ContainerAny[T,Con]): ContainerAnyOps[T,Con] = new ContainerAnyOps[T,Con](x)
  }

  trait VectorImplicits{
    implicit def VectorOps[@tbsp F : ClassTag, Size <: Nat](lhs : Vec[F,Size])(implicit vec : AlgVector[F, Size]): VectorsOps[F, Size] = new VectorsOps[F,Size](lhs)
  }

  object ContainerImplicits extends ContainerImplicits

  trait AllOps extends
  ContainerImplicits with
  VectorImplicits

}

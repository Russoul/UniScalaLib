package russoul.lib.common.math.geometry.simple

import russoul.lib.common._
import russoul.lib.common.math.geometry.simple.general.GeometricShape
import russoul.lib.common.Implicits._
import russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import shapeless.Nat
import shapeless.Nat._
import Abstraction._
/**
  * Created by russoul on 23.04.17.
  */
@immutable class Line2Over[V[_,_ <: Nat], @tbsp F]private(val start:V[F,_2], val end:V[F,_2]) extends GeometricShape[V,F,_2] {


  def genDir()(implicit ev1: CES[V,F,_2], ev2: T1[F,V,_2], field: Field[F]): V[F,_2] = (end - start).normalize()


  override def translate(v: V[F,_2])(implicit ev1: CES[V,F,_2], ev2: T1[F,V,_2], field: Field[F]): Line2Over[V,F] = {
    new Line2Over(start + v, end + v)
  }

  override def toString(): String = {
    "Line2(start = " + start + ";end = " + end + " )"

  }

  def scaleAroundBasis(scalar:F)(implicit ev1: CES[V,F,_2], ev2: T1[F,V,_2], field: Field[F]): Line2Over[V,F] =
  {
    new Line2Over(start * scalar, end * scalar)
  }

}

object Line2Over{
  def apply[V[_,_ <: Nat], @tbsp F](start:V[F,_2], end:V[F,_2]) = new Line2Over[V,F](start, end)
}
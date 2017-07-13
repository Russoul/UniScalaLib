package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.{immutable, tbsp}
import Russoul.lib.common.math.geometry.simple.general.Shape2
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import shapeless.Nat
import shapeless.Nat._

/**
  * Created by russoul on 23.04.17.
  */
@immutable class Line2Over[V[_,_ <: Nat], @tbsp F : Field]private(val start:V[F,_2], val end:V[F,_2])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_2], tensor1: Tensor1[F,V,_2])  extends Shape2[V[F,_2],F] {


  def genDir(): V[F,_2] = (end - start).normalize()


  override def translate(v: V[F,_2]): Line2Over[V,F] = {
    new Line2Over(start + v, end + v)
  }

  override def toString(): String = {
    "Line2(start = " + start + ";end = " + end + " )"

  }

  def scaleAroundBasis(scalar:F): Line2Over[V,F] =
  {
    new Line2Over(start * scalar, end * scalar)
  }

}

object Line2Over{
  def apply[V[_,_ <: Nat], @tbsp F : Field](start:V[F,_2], end:V[F,_2])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_2], tensor1: Tensor1[F,V,_2]) = new Line2Over[V,F](start, end)
}
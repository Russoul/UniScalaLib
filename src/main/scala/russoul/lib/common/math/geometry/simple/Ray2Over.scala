package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import russoul.lib.common._
import russoul.lib.common.math.geometry.simple.general.GeometricShape
import Implicits._
import shapeless.Nat
import shapeless.Nat._
import Abstraction._

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Ray2Over[V[_,_ <: Nat], @tbsp F]private(val start: V[F,_2],val dir: V[F,_2]) extends GeometricShape[V,F,_2] {

  override def translate(v: V[F,_2])(implicit ev1: CES[V,F,_2], ev2:T1[F,V,_2], field: Field[F]): Ray2Over[V,F] = {
    new Ray2Over(start + v, dir)
  }

  override def toString(): String = {
    "Ray2(start = " + start + ";dir = " + dir + " )"
  }

  override def scaleAroundBasis(factor: F)(implicit ev1: CES[V,F,_2], ev2:T1[F,V,_2], field: Field[F]): Ray2Over[V,F] = {
    new Ray2Over(start * factor, dir)
  }
}

object Ray2Over{
  def apply[V[_,_ <: Nat], @tbsp F](start: V[F,_2], dir: V[F,_2]) = new Ray2Over[V,F](start, dir)
}
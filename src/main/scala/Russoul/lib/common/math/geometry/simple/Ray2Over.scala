package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import Russoul.lib.common.{Implicits, immutable}
import Russoul.lib.common.math.geometry.simple.general.Shape2
import Implicits._
import shapeless.Nat._

/**
  * Created by russoul on 23.04.17.
  */
@immutable class Ray2Over[V[_,_], @specialized F : Field]private(val start: V[F,_2],val dir: V[F,_2])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_2])  extends Shape2[V[F,_2],F] {

  override def translate(v: V[F,_2]): Ray2Over[V,F] = {
    new Ray2Over(start + v, dir)
  }

  override def toString(): String = {
    "Ray2(start = " + start + ";dir = " + dir + " )"
  }

  override def scaleAroundBasis(factor: F): Ray2Over[V,F] = {
    new Ray2Over(start * factor, dir)
  }
}

object Ray2Over{
  def apply[V[_,_], @specialized F : Field](start: V[F,_2], dir: V[F,_2])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_2]) = new Ray2Over[V,F](start, dir)
}
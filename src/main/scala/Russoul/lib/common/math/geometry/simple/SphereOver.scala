package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import Russoul.lib.common.math.geometry.simple.general.CenteredShape3
import Russoul.lib.common.{immutable, tbsp}
import shapeless.Nat._
import Russoul.lib.common.Implicits._
import shapeless.Nat

/**
  * Created by Russoul on 21.04.2016.
  */
@immutable class SphereOver[V[_,_ <: Nat],@tbsp F : Field]private (val center:V[F,_3],val rad: F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3]) extends CenteredShape3[V[F,_3],F] {

  override def translate(v: V[F,_3]): SphereOver[V,F] = {
    new SphereOver(center + v, rad)
  }

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F): SphereOver[V,F] = {
    new SphereOver(center, factor * rad)
  }

  override def toString: String = {
    "Sphere(center = " + center.toString() + ";radius = " + rad + " )"
  }
}

object SphereOver{
  def apply[V[_,_ <: Nat],@tbsp F : Field](center:V[F,_3],rad: F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3]) = new SphereOver[V,F](center, rad)
}

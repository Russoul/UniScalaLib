package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.{immutable, tbsp}
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import shapeless.Nat
import shapeless.Nat._


@immutable class RayOver[V[_,_ <: Nat],@tbsp F : Field]private (val start: V[F,_3],val dir: V[F,_3])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3], tensor1:Tensor1[F,V,_3]) extends Shape3[V[F,_3],F] {

  override def translate(v: V[F,_3]): RayOver[V,F] = {
    new RayOver(start + v, dir)
  }

  override def toString(): String = {
    "Ray(start = " + start + ";dir = " + dir + ")"

  }
}

object RayOver
{

  def apply[V[_,_ <: Nat],@tbsp F : Field](pos: V[F,_3], look: V[F,_3], zNear: F, zFar: F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3], tensor1:Tensor1[F,V,_3]): RayOver[V,F] =
  {
    new RayOver(pos + look * zNear, look)
  }

  def apply[V[_,_ <: Nat],@tbsp F : Field](start: V[F,_3], dir: V[F,_3])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3], tensor1:Tensor1[F,V,_3]) = new RayOver[V,F](start, dir)
}

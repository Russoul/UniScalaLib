package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import shapeless.Nat._


@immutable class RayOver[V[_,_],@specialized F : Field]private (val start: V[F,_3],val dir: V[F,_3])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3]) extends Shape3[V[F,_3],F] {

  override def translate(v: V[F,_3]): RayOver[V,F] = {
    new RayOver(start + v, dir)
  }

  override def toString(): String = {
    "Ray(start = " + start + ";dir = " + dir + ")"

  }
}

object RayOver
{

  def apply[V[_,_],@specialized F : Field](pos: V[F,_3], look: V[F,_3], zNear: F, zFar: F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3]): RayOver[V,F] =
  {
    new RayOver(pos + look * zNear, look)
  }

  def apply[V[_,_],@specialized F : Field](start: V[F,_3], dir: V[F,_3])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3]) = new RayOver[V,F](start, dir)
}

package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Vec3
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField


@immutable case class RayOver[V,@specialized F](start: V, dir: V)(implicit ev: CanonicalEuclideanSpaceOverField[V,F]) extends Shape3[V,F] {
  assert(ev.dimensions == 3)

  override def translate(v: V): RayOver[V,F] = {
    RayOver(start + v, dir)
  }

  override def toString(): String = {
    "Ray(start = " + start + ";dir = " + dir + ")"

  }
}

object RayOver
{

  def apply[V,@specialized F](pos: V, look: V, zNear: F, zFar: F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F]): RayOver[V,F] =
  {
    new RayOver(pos + look * zNear, look)
  }
}

package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.EuclideanSpaceOverField.Implicits._
import Russoul.lib.common.TypeClasses.{EuclideanSpace3OverField, Field}
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.linear.Vec3


@immutable case class RayOverES[V,@specialized F](start: V, dir: V)(implicit ev: EuclideanSpace3OverField[V,F]) extends Shape3[V,F]
{


  override def translate(v: V): RayOverES[V,F] = {
    RayOverES(start + v, dir)
  }

  override def toString(): String = {
    "Ray(start = " + start + ";dir = " + dir + ")"

  }
}

object RayOverES
{

  def apply[V,@specialized F](pos: V, look: V, zNear: F, zFar: F)(implicit ev: EuclideanSpace3OverField[V,F]): RayOverES[V,F] =
  {
    new RayOverES(pos + look * zNear, look)
  }
}

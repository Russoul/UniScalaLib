package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.Vec3


@immutable case class Ray[A](start: Vec3[A], dir: Vec3[A])(implicit ev: Field[A]) extends Shape3[A]
{


  override def translate(v: Vec3[A]): Ray[A] = {
    Ray(start + v, dir)
  }

  override def toString(): String = {
    "Ray(start = " + start + ";dir = " + dir + ")"

  }
}

object Ray
{

  def apply[A](pos: Vec3[A], look: Vec3[A], zNear: A, zFar: A)(implicit ev: Field[A]): Ray[A] =
  {
    new Ray(pos + look * zNear, look)
  }
}

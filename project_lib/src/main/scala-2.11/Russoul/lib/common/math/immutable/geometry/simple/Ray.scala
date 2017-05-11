package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.vec3


@immutable case class Ray(start: vec3, dir: vec3) extends Shape3
{


  override def translate(v: vec3): Ray = {
    Ray(start + v, dir)
  }

  override def toString(): String = {
    "Ray(start = " + start + ";dir = " + dir + ")"

  }
}

object Ray
{

  def apply(pos: vec3, look: vec3, zNear: Float, zFar: Float): Ray =
  {
    new Ray(pos + look * zNear, look)
  }
}

package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.vec3


class Ray(val start: vec3, val dir: vec3)
{
}

object Ray
{

  def apply(pos: vec3, look: vec3, zNear: Float, zFar: Float): Ray =
  {
    new Ray(pos + look * zNear, look)
  }
}

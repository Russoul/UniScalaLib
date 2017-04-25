package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.vec3

/**
  * Created by Russoul on 23.07.2016.
  */
class Triangle(val p1:vec3, val p2:vec3, val p3:vec3)
{
  override def toString: String =
  {
    "Triangle( point1 = " + p1 + "; point2 = " + p2 + "; point3 = " + p3 + " )"
  }
}

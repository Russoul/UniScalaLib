package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.vec2


/**
  * Created by russoul on 23.04.17.
  */
class Ray2(val start: vec2, val dir: vec2)
{
  override def toString(): String =
  {
    "Ray2( start = " + start + "; dir = " + dir + " )"

  }
}

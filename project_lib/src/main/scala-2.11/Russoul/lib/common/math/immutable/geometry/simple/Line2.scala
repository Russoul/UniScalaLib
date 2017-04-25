package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.vec2

/**
  * Created by russoul on 23.04.17.
  */
class Line2(val start:vec2, val end:vec2)
{
  def genDir(): vec2 = (end - start).normalize()

  override def toString(): String =
  {
    "Line2( start = " + start + "; end = " + end + " )"

  }

}

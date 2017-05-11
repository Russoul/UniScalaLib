package Russoul.lib.common.math.immutable.geometry.simple.general

import Russoul.lib.common.math.immutable.linear.vec3

/**
  * Created by russoul on 11.05.17.
  */
trait Shape3
{
  def translate(v:vec3): Shape3
}

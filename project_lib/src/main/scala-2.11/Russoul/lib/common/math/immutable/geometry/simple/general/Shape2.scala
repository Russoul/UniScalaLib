package Russoul.lib.common.math.immutable.geometry.simple.general

import Russoul.lib.common.math.immutable.linear.vec2

/**
  * Created by russoul on 11.05.17.
  */
trait Shape2 {
  def translate(v:vec2): Shape2
  def scaleAroundBasis(factor:Float):Shape2
}

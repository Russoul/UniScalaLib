package Russoul.lib.common.math.immutable.geometry.simple.general

import Russoul.lib.common.math.immutable.linear.Vec2

/**
  * Created by russoul on 11.05.17.
  */
trait Shape2[A] {
  def translate(v:Vec2[A]): Shape2[A]
  def scaleAroundBasis(factor:A):Shape2[A]
}

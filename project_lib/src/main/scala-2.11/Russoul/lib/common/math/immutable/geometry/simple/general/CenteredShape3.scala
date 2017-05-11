package Russoul.lib.common.math.immutable.geometry.simple.general

import Russoul.lib.common.math.immutable.linear.vec3

/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape3 extends Shape3
{
  def center:vec3

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  def scale(factor:Float):CenteredShape3
}

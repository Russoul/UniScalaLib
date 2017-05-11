package Russoul.lib.common.math.immutable.geometry.simple.general

import Russoul.lib.common.math.immutable.linear.vec2

/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape2 extends Shape2 {

  def center:vec2

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  def scale(factor:Float):CenteredShape2



}

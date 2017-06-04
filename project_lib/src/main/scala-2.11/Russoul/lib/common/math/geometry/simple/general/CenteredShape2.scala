package Russoul.lib.common.math.geometry.simple.general

import Russoul.lib.common.math.linear.Vec2

/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape2[V,F] extends Shape2[V,F] {

  def center:V

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  def scale(factor:F):CenteredShape2[V,F]



}

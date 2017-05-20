package Russoul.lib.common.math.immutable.geometry.simple.general

import Russoul.lib.common.math.immutable.linear.Vec2

/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape2[A] extends Shape2[A] {

  def center:Vec2[A]

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  def scale(factor:A):CenteredShape2[A]



}

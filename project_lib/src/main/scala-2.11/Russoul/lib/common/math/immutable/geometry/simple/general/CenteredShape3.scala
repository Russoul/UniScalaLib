package Russoul.lib.common.math.immutable.geometry.simple.general

import Russoul.lib.common.math.immutable.linear.Vec3

/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape3[A] extends Shape3[A]
{
  def center:Vec3[A]

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  def scale(factor:A):CenteredShape3[A]
}

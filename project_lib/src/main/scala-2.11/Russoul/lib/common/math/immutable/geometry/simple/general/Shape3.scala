package Russoul.lib.common.math.immutable.geometry.simple.general

import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.immutable.linear.Vec3

/**
  * Created by russoul on 11.05.17.
  */
trait Shape3[A]
{
  def translate(v:Vec3[A]): Shape3[A]
}

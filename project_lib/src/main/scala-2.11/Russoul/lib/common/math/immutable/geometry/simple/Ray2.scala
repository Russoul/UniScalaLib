package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.Shape2
import Russoul.lib.common.math.immutable.linear.vec2


/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Ray2(start: vec2, dir: vec2) extends Shape2
{


  override def translate(v: vec2): Shape2 = {
    Ray2(start + v, dir)
  }

  override def toString(): String = {
    "Ray2(start = " + start + ";dir = " + dir + " )"
  }

  override def scaleAroundBasis(factor: Float): Ray2 = {
    Ray2(start * factor, dir)
  }
}

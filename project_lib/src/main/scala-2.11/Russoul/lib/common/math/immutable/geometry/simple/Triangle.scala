package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.vec3

/**
  * Created by Russoul on 23.07.2016.
  */
@immutable case class Triangle(p1:vec3, p2:vec3, p3:vec3) extends Shape3 {


  override def translate(v: vec3): Triangle = {
    Triangle(p1 + v, p2 + v, p3 + v)
  }

  override def toString: String = {
    "Triangle(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

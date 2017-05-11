package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.vec3

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class Plane(point:vec3, normal:vec3) extends Shape3{

  override def translate(v: vec3): Plane = copy()

  override def toString(): String = {
    "Plane( point = " + point + "; normal = " + normal + " )"

  }


}

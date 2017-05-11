package Russoul.lib.common.math.immutable.geometry.simple


import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape3
import Russoul.lib.common.math.immutable.linear.vec3


/**
  * Created by Russoul on 21.04.2016.
  */
@immutable case class Sphere(center:vec3, rad: Float) extends CenteredShape3 {




  override def translate(v: vec3): Sphere = {
    Sphere(center + v, rad)
  }

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: Float): Sphere = {
    Sphere(center, factor * rad)
  }

  override def toString: String = {
    "Sphere(center = " + center.toString() + ";radius = " + rad + " )"
  }
}

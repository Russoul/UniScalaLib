package Russoul.lib.common.math.immutable.geometry.simple


import Russoul.lib.common.math.immutable.linear.vec3


/**
  * Created by Russoul on 21.04.2016.
  */
class Sphere(val center:vec3, val rad: Float)
{
  override def toString: String =
  {
    "Sphere( center = " + center.toString() + "; radius = " + rad + " )"
  }
}

package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.{vec2, vec3}
import Russoul.lib.common.utils.vector

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
class Square2 (val center:vec2, val extent:Float){


  def genVertices() = vector[vec2](center - vec2(extent,extent), center + vec2(extent, -extent), center + vec2(extent,extent), center + vec2(-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:Float): Rectangle2 =
  {
    new Rectangle2(center, vec2(extent,extent) * scalar)
  }

  def toRectangle2():Rectangle2 =
  {
    new Rectangle2(center, vec2(extent, extent))
  }

  override def toString: String =
  {
    "Square2( center = "+center.toString() + "; extent = " + extent + " )"
  }

}

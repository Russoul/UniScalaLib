package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.{vec2, vec3}
import Russoul.lib.common.utils.vector

/**
  * Created by russoul on 11.03.17.
  */
class Rectangle2 (val center:vec2, val extent:vec2){


  def genVertices() = vector[vec2](center - extent, center + vec2(extent.x, -extent.y), center + extent, center + vec2(-extent.x, extent.y))

  def toRectangleParallelToZ(zLevel:Float): Rectangle =
  {
    new Rectangle(vec3(center, zLevel), vec3(extent.x, 0,0), vec3(0,extent.y, 0))
  }

  override def toString: String =
  {
    "Rectangle2( "+center.toString() + ";" + extent.toString() + " )"
  }
}

object Rectangle2
{
  def fromMinMax(min:vec2, max:vec2):Rectangle2 =
  {
    val t = (max - min)*0.5F
    new Rectangle2(min + t, t)
  }
}

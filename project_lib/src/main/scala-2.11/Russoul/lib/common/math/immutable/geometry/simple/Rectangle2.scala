package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.immutable.linear.{vec2, vec3}
import Russoul.lib.common.utils.vector

/**
  * Created by russoul on 11.03.17.
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Rectangle2 (center:vec2, extent:vec2) extends CenteredShape2{


  override def translate(v: vec2): Rectangle2 = {
    Rectangle2(center + v, extent)
  }

  def genVertices(): vector[vec2] = vector[vec2](center - extent, center + vec2(extent.x, -extent.y), center + extent, center + vec2(-extent.x, extent.y))

  def toRectangleParallelToZ(zLevel:Float): Rectangle =
  {
    new Rectangle(vec3(center, zLevel), vec3(extent.x, 0,0), vec3(0,extent.y, 0))
  }

  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:Float): Rectangle2 =
  {
    new Rectangle2(center, extent * scalar)
  }

  def scaleAroundBasis(scalar:Float):Rectangle2 =
  {
    new Rectangle2(center * scalar, extent * scalar)
  }

  override def toString: String =
  {
    "Rectangle2(center = "+center.toString() + ";extent = " + extent.toString() + ")"
  }
}

object Rectangle2
{
  def fromMinMax(min:vec2, max:vec2):Rectangle2 =
  {
    val t = (max - min)*0.5F
    Rectangle2(min + t, t)
  }
}

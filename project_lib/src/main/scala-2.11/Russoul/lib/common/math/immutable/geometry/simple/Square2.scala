package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.immutable.linear.{vec2, vec3}
import Russoul.lib.common.utils.vector

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Square2 (center:vec2, extent:Float) extends CenteredShape2{




  override def translate(v: vec2): Square2 = {
    Square2(center + v, extent)
  }

  override def scaleAroundBasis(factor: Float) = {
    Square2(center * factor, extent * factor)
  }

  def genVertices(): vector[vec2] = vector[vec2](center - vec2(extent,extent), center + vec2(extent, -extent), center + vec2(extent,extent), center + vec2(-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:Float): Square2 =
  {
    Square2(center, extent * scalar)
  }

  def toRectangle2():Rectangle2 =
  {
    Rectangle2(center, vec2(extent, extent))
  }

  override def toString: String =
  {
    "Square2(center = "+center.toString() + ";extent = " + extent + ")"
  }

}

package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.vec2

/**
  * Created by russoul on 23.04.17.
  */
class Circle(val center:vec2, val rad:Float)
{

  def scaleAroundBasis(scalar:Float): Circle =
  {
    new Circle(center * scalar, rad * scalar)
  }

  def inscribedInRectangle2(): Rectangle2 =
  {
    new Rectangle2(center, vec2(rad,rad))
  }

  override def toString(): String =
  {
    "Circle( center = " + center + "; radius = " + rad + " )"
  }
}

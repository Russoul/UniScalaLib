package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.immutable.linear.vec2

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Circle(center:vec2, rad:Float) extends CenteredShape2
{

  override def translate(v: vec2): CenteredShape2 = {
    Circle(center + v, rad)
  }

  override def scale(scalar:Float): Circle = {
    Circle(center, rad * scalar)
  }

  override def scaleAroundBasis(scalar:Float): Circle = {
    Circle(center * scalar, rad * scalar)
  }

  def inscribedInRectangle2(): Rectangle2 = {
    new Rectangle2(center, vec2(rad,rad))
  }

  override def toString(): String =
  {
    "Circle( center = " + center + "; radius = " + rad + " )"
  }
}

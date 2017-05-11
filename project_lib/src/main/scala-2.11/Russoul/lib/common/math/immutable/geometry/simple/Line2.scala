package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.Shape2
import Russoul.lib.common.math.immutable.linear.vec2

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Line2(start:vec2, end:vec2) extends Shape2
{
  def genDir(): vec2 = (end - start).normalize()


  override def translate(v: vec2): Line2 = {
    Line2(start + v, end + v)
  }

  override def toString(): String = {
    "Line2(start = " + start + ";end = " + end + " )"

  }

  def scaleAroundBasis(scalar:Float): Line2 =
  {
    Line2(start * scalar, end * scalar)
  }

}

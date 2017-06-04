package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.EuclideanSpaceOverField.Implicits._
import Russoul.lib.common.TypeClasses.{EuclideanSpace2OverField, Field}
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.Shape2
import Russoul.lib.common.math.linear.Vec2

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Line2OverES[V, @specialized F](start:V, end:V)(implicit ev: EuclideanSpace2OverField[V,F])  extends Shape2[V,F]
{
  def genDir(): V = (end - start).normalize()


  override def translate(v: V): Line2OverES[V,F] = {
    Line2OverES(start + v, end + v)
  }

  override def toString(): String = {
    "Line2(start = " + start + ";end = " + end + " )"

  }

  def scaleAroundBasis(scalar:F): Line2OverES[V,F] =
  {
    Line2OverES(start * scalar, end * scalar)
  }

}

package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{EuclideanSpace2OverField, Field}
import Russoul.lib.common.TypeClasses.Field.Implicits._
import Russoul.lib.common.TypeClasses.EuclideanSpaceOverField.Implicits._
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.linear.Vec2

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class CircleOverES[V,@specialized F](center:V, rad:F)(implicit ev : EuclideanSpace2OverField[V,F]) extends CenteredShape2[V,F]
{

  override def translate(v: V): CenteredShape2[V,F] = {
    CircleOverES(center + v, rad)
  }

  override def scale(scalar:F): CircleOverES[V,F] = {
    CircleOverES(center, rad * scalar)
  }

  override def scaleAroundBasis(scalar:F): CircleOverES[V,F] = {
    CircleOverES(center * scalar, rad * scalar)
  }

  def inscribedInRectangle2(): Rectangle2OverES[V,F] = {
    new Rectangle2OverES(center, ev.create(rad,rad))
  }

  override def toString(): String =
  {
    "Circle( center = " + center + "; radius = " + rad + " )"
  }
}

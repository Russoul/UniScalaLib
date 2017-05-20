package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.immutable.linear.Vec2

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Circle[A](center:Vec2[A], rad:A)(implicit ev : FieldLike[A]) extends CenteredShape2[A]
{

  override def translate(v: Vec2[A]): CenteredShape2[A] = {
    Circle(center + v, rad)
  }

  override def scale(scalar:A): Circle[A] = {
    Circle(center, rad * scalar)
  }

  override def scaleAroundBasis(scalar:A): Circle[A] = {
    Circle(center * scalar, rad * scalar)
  }

  def inscribedInRectangle2(): Rectangle2[A] = {
    new Rectangle2(center, Vec2(rad,rad))
  }

  override def toString(): String =
  {
    "Circle( center = " + center + "; radius = " + rad + " )"
  }
}

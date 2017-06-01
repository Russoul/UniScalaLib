package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.immutable.geometry.simple.general.Shape2
import Russoul.lib.common.math.immutable.linear.Vec2

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Line2[A](start:Vec2[A], end:Vec2[A])(implicit ev: Field[A])  extends Shape2[A]
{
  def genDir(): Vec2[A] = (end - start).normalize()


  override def translate(v: Vec2[A]): Line2[A] = {
    Line2(start + v, end + v)
  }

  override def toString(): String = {
    "Line2(start = " + start + ";end = " + end + " )"

  }

  def scaleAroundBasis(scalar:A): Line2[A] =
  {
    Line2(start * scalar, end * scalar)
  }

}

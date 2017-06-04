package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.EuclideanSpaceOverField.Implicits._
import Russoul.lib.common.TypeClasses.{EuclideanSpace2OverField, Field}
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.Shape2
import Russoul.lib.common.math.linear.Vec2


/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Ray2OverES[V, @specialized F](start: V, dir: V)(implicit ev: EuclideanSpace2OverField[V,F])  extends Shape2[V,F]
{


  override def translate(v: V): Shape2[V,F] = {
    Ray2OverES(start + v, dir)
  }

  override def toString(): String = {
    "Ray2(start = " + start + ";dir = " + dir + " )"
  }

  override def scaleAroundBasis(factor: F): Ray2OverES[V,F] = {
    Ray2OverES(start * factor, dir)
  }
}

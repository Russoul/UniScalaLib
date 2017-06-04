package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{EuclideanSpaceOverField, EuclideanSpace3OverField, Field, VectorSpaceOverField}
import Russoul.lib.common.{Real, Real3, immutable}
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.linear.Vec3
import Russoul.lib.common.TypeClasses.EuclideanSpaceOverField.Implicits._

/**
  * Created by Russoul on 23.07.2016.
  */
@immutable case class TriangleOverES[V, @specialized F](p1:V, p2:V, p3:V)(implicit ev: EuclideanSpace3OverField[V,F]) extends Shape3[V,F] {



  override def translate(v: V): TriangleOverES[V,F] = {
    TriangleOverES(p1 + v, p2 + v, p3 + v)
  }

  override def toString: String = {
    "Triangle(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

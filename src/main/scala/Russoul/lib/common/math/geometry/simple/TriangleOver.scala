package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.{Real, Real3, immutable}
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.math.algebra.Vec3
import Russoul.lib.common.Implicits._
import shapeless.Nat

/**
  * Created by Russoul on 23.07.2016.
  */
@immutable case class TriangleOver[V, @specialized F](p1:V, p2:V, p3:V)(implicit ev: CanonicalEuclideanSpaceOverField[V,F,Nat._3]) extends Shape3[V,F] {


  override def translate(v: V): TriangleOver[V,F] = {
    TriangleOver(p1 + v, p2 + v, p3 + v)
  }

  override def toString: String = {
    "Triangle(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

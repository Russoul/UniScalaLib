package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.{Implicits, immutable}
import Russoul.lib.common.math.algebra.Vec2
import Russoul.lib.common.math.geometry.simple.general.Shape2
import Implicits._
/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Ray2Over[V, @specialized F](start: V, dir: V)(implicit ev: CanonicalEuclideanSpaceOverField[V,F])  extends Shape2[V,F] {
  assert(ev.dimensions == 2)

  override def translate(v: V): Shape2[V,F] = {
    Ray2Over(start + v, dir)
  }

  override def toString(): String = {
    "Ray2(start = " + start + ";dir = " + dir + " )"
  }

  override def scaleAroundBasis(factor: F): Ray2Over[V,F] = {
    Ray2Over(start * factor, dir)
  }
}

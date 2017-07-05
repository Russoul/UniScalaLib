package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.{Mat, Vec2}
import Russoul.lib.common.math.geometry.simple.general.Shape2
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Line2Over[V, @specialized F : Field](start:V, end:V)(implicit ev: CanonicalEuclideanSpaceOverField[V,F])  extends Shape2[V,F] {
  assert(ev.dimensions == 2)


  def genDir(): V = (end - start).normalize()


  override def translate(v: V): Line2Over[V,F] = {
    Line2Over(start + v, end + v)
  }

  override def toString(): String = {
    "Line2(start = " + start + ";end = " + end + " )"

  }

  def scaleAroundBasis(scalar:F): Line2Over[V,F] =
  {
    Line2Over(start * scalar, end * scalar)
  }

}

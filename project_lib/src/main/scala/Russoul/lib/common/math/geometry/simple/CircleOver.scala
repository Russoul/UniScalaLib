package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Vec2
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.Implicits._

import scala.reflect.ClassTag

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class CircleOver[V : ClassTag,@specialized F : Field](center:V, rad:F)(implicit ev : CanonicalEuclideanSpaceOverField[V,F]) extends CenteredShape2[V,F] {
  assert(ev.dimensions == 2)


  override def translate(v: V): CenteredShape2[V,F] = {
    CircleOver(center + v, rad)
  }

  override def scale(scalar:F): CircleOver[V,F] = {
    CircleOver(center, rad * scalar)
  }

  override def scaleAroundBasis(scalar:F): CircleOver[V,F] = {
    CircleOver(center * scalar, rad * scalar)
  }

  def inscribedInRectangle2(): Rectangle2Over[V,F] = {
    new Rectangle2Over[V,F](center, ev.create(rad,rad))
  }

  override def toString(): String =
  {
    "Circle( center = " + center + "; radius = " + rad + " )"
  }
}

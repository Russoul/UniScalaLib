package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import Russoul.lib.common.math.geometry.simple.general.CenteredShape3
import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Vec3
import Russoul.lib.common.Implicits._

/**
  * Created by Russoul on 21.04.2016.
  */
@immutable case class SphereOver[V,@specialized F : Field](center:V, rad: F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F]) extends CenteredShape3[V,F] {
  assert(ev.dimensions == 3)

  override def translate(v: V): SphereOver[V,F] = {
    SphereOver(center + v, rad)
  }

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F): SphereOver[V,F] = {
    SphereOver(center, factor * rad)
  }

  override def toString: String = {
    "Sphere(center = " + center.toString() + ";radius = " + rad + " )"
  }
}

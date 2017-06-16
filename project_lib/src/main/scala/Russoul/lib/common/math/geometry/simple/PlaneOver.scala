package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Vec3
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class PlaneOver[V,@specialized F](point:V, normal:V)(implicit ev : CanonicalEuclideanSpaceOverField[V,F]) extends Shape3[V,F]{
  assert(ev.dimensions == 3)


  override def translate(v: V): PlaneOver[V,F] = copy()

  override def toString(): String = {
    "Plane( point = " + point + "; normal = " + normal + " )"

  }


}

package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{EuclideanSpace3OverField, Field}
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.linear.Vec3

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class PlaneOverES[V,@specialized F](point:V, normal:V)(implicit ev : EuclideanSpace3OverField[V,F]) extends Shape3[V,F]{

  override def translate(v: V): PlaneOverES[V,F] = copy()

  override def toString(): String = {
    "Plane( point = " + point + "; normal = " + normal + " )"

  }


}

package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{EuclideanSpace3OverField, EuclideanSpaceOverField, Field}
import Russoul.lib.common.math.geometry.simple.general.CenteredShape3
import Russoul.lib.common.math.linear.Vec3
import Field.Implicits._
import EuclideanSpaceOverField.Implicits._
import Russoul.lib.common.immutable

/**
  * Created by Russoul on 21.04.2016.
  */
@immutable case class SphereOverES[V,F](center:V, @specialized rad: F)(implicit ev: EuclideanSpace3OverField[V,F]) extends CenteredShape3[V,F] {


  override def translate(v: V): SphereOverES[V,F] = {
    SphereOverES(center + v, rad)
  }

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F): SphereOverES[V,F] = {
    SphereOverES(center, factor * rad)
  }

  override def toString: String = {
    "Sphere(center = " + center.toString() + ";radius = " + rad + " )"
  }
}

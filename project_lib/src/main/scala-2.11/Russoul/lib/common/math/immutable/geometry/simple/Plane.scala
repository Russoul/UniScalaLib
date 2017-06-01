package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.Vec3

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class Plane[A](point:Vec3[A], normal:Vec3[A])(implicit ev : Field[A]) extends Shape3[A]{

  override def translate(v: Vec3[A]): Plane[A] = copy()

  override def toString(): String = {
    "Plane( point = " + point + "; normal = " + normal + " )"

  }


}

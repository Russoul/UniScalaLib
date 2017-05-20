package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.Vec3

/**
  * Created by Russoul on 23.07.2016.
  */
@immutable case class Triangle[A](p1:Vec3[A], p2:Vec3[A], p3:Vec3[A])(implicit ev: FieldLike[A]) extends Shape3[A] {


  override def translate(v: Vec3[A]): Triangle[A] = {
    Triangle(p1 + v, p2 + v, p3 + v)
  }

  override def toString: String = {
    "Triangle(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

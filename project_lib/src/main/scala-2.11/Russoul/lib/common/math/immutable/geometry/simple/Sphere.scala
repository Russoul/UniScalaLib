package Russoul.lib.common.math.immutable.geometry.simple


import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape3
import Russoul.lib.common.math.immutable.linear.Vec3

import Field.Implicits._

/**
  * Created by Russoul on 21.04.2016.
  */
@immutable case class Sphere[A : Field](center:Vec3[A], rad: A) extends CenteredShape3[A] {




  override def translate(v: Vec3[A]): Sphere[A] = {
    Sphere(center + v, rad)
  }

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: A): Sphere[A] = {
    Sphere(center, factor * rad)
  }

  override def toString: String = {
    "Sphere(center = " + center.toString() + ";radius = " + rad + " )"
  }
}

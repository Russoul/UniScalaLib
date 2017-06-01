package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.immutable.geometry.simple.general.Shape2
import Russoul.lib.common.math.immutable.linear.Vec2


/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Ray2[A](start: Vec2[A], dir: Vec2[A])(implicit ev: Field[A])  extends Shape2[A]
{


  override def translate(v: Vec2[A]): Shape2[A] = {
    Ray2(start + v, dir)
  }

  override def toString(): String = {
    "Ray2(start = " + start + ";dir = " + dir + " )"
  }

  override def scaleAroundBasis(factor: A): Ray2[A] = {
    Ray2(start * factor, dir)
  }
}

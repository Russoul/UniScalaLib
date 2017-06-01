package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.TypeClasses.Field.Implicits._
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.immutable.linear.{Vec2, Vec3}
import Russoul.lib.common.utils.Arr

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Square2 [A](center:Vec2[A], extent:A)(implicit ev: Field[A])  extends CenteredShape2[A]{




  override def translate(v: Vec2[A]): Square2[A] = {
    Square2(center + v, extent)
  }

  override def scaleAroundBasis(factor: A): Square2[A] = {
    Square2(center * factor, extent * factor)
  }

  def genVertices(): Arr[Vec2[A]] = Arr(center - Vec2(extent,extent), center + Vec2(extent, -extent), center + Vec2(extent,extent), center + Vec2(-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:A): Square2[A] =
  {
    Square2(center, extent * scalar)
  }

  def toRectangle2():Rectangle2[A] =
  {
    Rectangle2(center, Vec2(extent, extent))
  }

  override def toString: String =
  {
    "Square2(center = "+center.toString() + ";extent = " + extent + ")"
  }

}

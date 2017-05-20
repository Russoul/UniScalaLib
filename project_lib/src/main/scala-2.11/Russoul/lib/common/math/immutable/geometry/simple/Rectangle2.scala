package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.immutable.linear.{Vec2, Vec3}
import Russoul.lib.common.utils.Vector

/**
  * Created by russoul on 11.03.17.
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Rectangle2[A](center:Vec2[A], extent:Vec2[A])(implicit ev : FieldLike[A]) extends CenteredShape2[A]{


  override def translate(v: Vec2[A]): Rectangle2[A] = {
    Rectangle2(center + v, extent)
  }

  def genVertices(): Vector[Vec2[A]] = Vector[Vec2[A]](center - extent, center + Vec2(extent.x, -extent.y), center + extent, center + Vec2(-extent.x, extent.y))

  def toRectangleParallelToZ(zLevel:A): Rectangle[A] =
  {
    new Rectangle(Vec3(center, zLevel), Vec3(extent.x, 0D,0D), Vec3(0D,extent.y, 0D))
  }

  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:A): Rectangle2[A] =
  {
    new Rectangle2(center, extent * scalar)
  }

  def scaleAroundBasis(scalar:A):Rectangle2[A] =
  {
    new Rectangle2(center * scalar, extent * scalar)
  }

  override def toString: String =
  {
    "Rectangle2(center = "+center.toString() + ";extent = " + extent.toString() + ")"
  }
}

object Rectangle2
{
  def fromMinMax[A](min:Vec2[A], max:Vec2[A])(implicit ev : FieldLike[A]):Rectangle2[A] =
  {
    val t = (max - min)*ev.fromDouble(0.5D)
    Rectangle2(min + t, t)
  }
}

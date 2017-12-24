package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.geometry.simple.general.{CenteredShape}
import russoul.lib.common.utils.Arr

import scala.reflect.ClassTag


import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 11.03.17.
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Rectangle2Over[@tbsp F]private(override val center:Vec2[F],val extent:Vec2[F]) extends CenteredShape[F,_2]{

  override def translate(v: Vec2[F])(implicit ev3 : Field[F], evTag: ClassTag[F]): Rectangle2Over[F] = {
    new Rectangle2Over(center + v, extent)
  }

  def genVertices()(implicit evTag: ClassTag[F], ev3 : Field[F]): Array[Vec2[F]] = Array[Vec2[F]](center - extent, center + Vec2[F](extent(0), -extent(1)), center + extent, center + Vec2[F](-extent(0), extent(1)))

  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F)(implicit ev3 : Field[F], tag : ClassTag[F]): Rectangle2Over[F] =
  {
    new Rectangle2Over(center, extent :* scalar)
  }

  def scaleAroundBasis(scalar:F)(implicit ev3 : Field[F], tag : ClassTag[F]):Rectangle2Over[F] =
  {
    new Rectangle2Over(center :* scalar, extent :* scalar)
  }

  override def toString: String =
  {
    "Rectangle2(center = "+center.toString() + ";extent = " + extent.toString() + ")"
  }


  def toRectangleParallelToZ(zLevel:F)(implicit field : Field[F], tag : ClassTag[F]): RectangleOver[F] =
  {
    RectangleOver[F](Vec3[F](center(0), center(1), zLevel), Vec3[F](extent(0), field.zero, field.zero), Vec3[F](field.zero,extent(2), field.zero))
  }


}

object Rectangle2Over
{
  def fromMinMax[@tbsp F : ClassTag](min:Vec2[F], max:Vec2[F])(implicit field : Field[F]):Rectangle2Over[F] =
  {
    val t = (max - min) :* field.fromDouble(0.5D)
    Rectangle2Over[F](min + t, t)
  }

  def apply[@tbsp F](center:Vec2[F], extent:Vec2[F]) = new Rectangle2Over[F](center, extent)
}

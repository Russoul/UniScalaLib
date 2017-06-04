package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{ConvertibleFromDouble, EuclideanSpace2OverField, EuclideanSpace3OverField, EuclideanSpaceOverField, Field}
import Russoul.lib.common.TypeClasses.Field.Implicits._
import Russoul.lib.common.TypeClasses.EuclideanSpaceOverField.Implicits._
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.linear.{Vec2, Vec3}
import Russoul.lib.common.utils.Arr

/**
  * Created by russoul on 11.03.17.
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Rectangle2OverES[V,F](center:V, extent:V)(implicit ev : EuclideanSpace2OverField[V,F]) extends CenteredShape2[V,F]{


  override def translate(v: V): Rectangle2OverES[V,F] = {
    Rectangle2OverES(center + v, extent)
  }

  def genVertices(): Arr[V] = new Arr[V](center - extent, center + ev.create(extent.x, -extent.y), center + extent, center + ev.create(-extent.x, extent.y))

  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F): Rectangle2OverES[V,F] =
  {
    new Rectangle2OverES(center, extent * scalar)
  }

  def scaleAroundBasis(scalar:F):Rectangle2OverES[V,F] =
  {
    new Rectangle2OverES(center * scalar, extent * scalar)
  }

  override def toString: String =
  {
    "Rectangle2(center = "+center.toString() + ";extent = " + extent.toString() + ")"
  }


  def toRectangleParallelToZ[V3](zLevel:F)(implicit v3: EuclideanSpace3OverField[V3,F] with ConvertibleFromDouble[F]): RectangleOverES[V3,F] =
  {
    new RectangleOverES(v3.create(center.x, center.y, zLevel), v3.create(extent.x, 0D.to[F],0D.to[F]), v3.create(0D.to[F],extent.y, 0D.to[F]))
  }


}

object Rectangle2OverES
{
  def fromMinMax[V,@specialized F](min:V, max:V)(implicit ev : EuclideanSpace2OverField[V,F] with ConvertibleFromDouble[F]):Rectangle2OverES[V,F] =
  {
    val t = (max - min)*ev.fromDouble(0.5D)
    Rectangle2OverES(min + t, t)
  }
}

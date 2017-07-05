package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.TypeClasses.Field
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.utils.Arr
import Russoul.lib.common.Implicits._

import scala.reflect.ClassTag

/**
  * Created by russoul on 11.03.17.
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Rectangle2Over[V : ClassTag, @specialized F : Field](center:V, extent:V)(implicit ev : CanonicalEuclideanSpaceOverField[V,F]) extends CenteredShape2[V,F]{
  assert(ev.dimensions == 2)

  override def translate(v: V): Rectangle2Over[V,F] = {
    Rectangle2Over(center + v, extent)
  }

  def genVertices(): Arr[V] = new Arr[V](center - extent, center + ev.create(extent.x, -extent.y), center + extent, center + ev.create(-extent.x, extent.y))

  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F): Rectangle2Over[V,F] =
  {
    new Rectangle2Over(center, extent * scalar)
  }

  def scaleAroundBasis(scalar:F):Rectangle2Over[V,F] =
  {
    new Rectangle2Over(center * scalar, extent * scalar)
  }

  override def toString: String =
  {
    "Rectangle2(center = "+center.toString() + ";extent = " + extent.toString() + ")"
  }


  def toRectangleParallelToZ[V3 : ClassTag](zLevel:F)(implicit v3: CanonicalEuclideanSpaceOverField[V3,F] with CanonicalCrossProductOp[V3]): RectangleOver[V3,F] =
  {
    new RectangleOver[V3,F](v3.create(center.x, center.y, zLevel), v3.create(extent.x, v3.scalar.zero,v3.scalar.zero), v3.create(v3.scalar.zero,extent.y, v3.scalar.zero))
  }


}

object Rectangle2Over
{
  def fromMinMax[V : ClassTag,@specialized F : Field](min:V, max:V)(implicit ev : CanonicalEuclideanSpaceOverField[V,F] , c: ConvertibleFromDouble[F]):Rectangle2Over[V,F] =
  {
    val t = (max - min)*c.fromDouble(0.5D)
    Rectangle2Over(min + t, t)
  }
}

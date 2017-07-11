package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.TypeClasses.Field
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.utils.Arr
import Russoul.lib.common.Implicits._
import shapeless.Nat._
import scala.reflect.ClassTag

/**
  * Created by russoul on 11.03.17.
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Rectangle2Over[V[_,_], @specialized F : ClassTag : Field](val center:V[F,_2],val extent:V[F,_2])(implicit ev : CanonicalEuclideanSpaceOverField[V,F,_2]) extends CenteredShape2[V[F,_2],F]{

  override def translate(v: V[F,_2]): Rectangle2Over[V,F] = {
    Rectangle2Over(center + v, extent)
  }

  def genVertices(): Array[V[F,_2]] = Array[V[F,_2]](center - extent, center + ev.staticContainer.factory.makeVector[_2](extent.x, -extent.y), center + extent, center + ev.staticContainer.factory.makeVector[_2](-extent.x, extent.y))

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


  def toRectangleParallelToZ(zLevel:F)(implicit v3: CanonicalEuclideanSpaceOverField[V,F,_3]): RectangleOver[V,F] =
  {
    new RectangleOver[V,F](ev.staticContainer.factory.makeVector[_3](center.x, center.y, zLevel), ev.staticContainer.factory.makeVector[_3](extent.x, v3.scalar.zero,v3.scalar.zero), ev.staticContainer.factory.makeVector[_3](v3.scalar.zero,extent.y, v3.scalar.zero))
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

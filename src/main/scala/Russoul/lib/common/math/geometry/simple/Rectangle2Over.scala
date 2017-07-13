package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.TypeClasses.Field
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.utils.Arr
import Russoul.lib.common.Implicits._
import shapeless.Nat
import shapeless.Nat._

import scala.reflect.ClassTag

/**
  * Created by russoul on 11.03.17.
  *
  * AXIS ALIGNED !!!
  */
@immutable class Rectangle2Over[V[_,_ <: Nat], @specialized F : ClassTag : Field]private(val center:V[F,_2],val extent:V[F,_2])(implicit evTag: ClassTag[V[F,_2]], ev : CanonicalEuclideanSpaceOverField[V,F,_2], tensor1: Tensor1[F,V,_2]) extends CenteredShape2[V[F,_2],F]{

  override def translate(v: V[F,_2]): Rectangle2Over[V,F] = {
    new Rectangle2Over(center + v, extent)
  }

  def genVertices(): Array[V[F,_2]] = Array[V[F,_2]](center - extent, center + ev.tensor1.make(extent.x, -extent.y), center + extent, center + ev.tensor1.make(-extent.x, extent.y))

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


  def toRectangleParallelToZ(zLevel:F)(implicit evTag3 : ClassTag[V[F,_3]], v3: CanonicalEuclideanSpaceOverField[V,F,_3], tensor11: Tensor1[F,V,_3], cross: CrossProductOverCanonicalEuclideanSpaceOverField[V,F]): RectangleOver[V,F] =
  {
    RectangleOver[V,F](v3.tensor1.make(center.x, center.y, zLevel), v3.tensor1.make(extent.x, v3.scalar.zero,v3.scalar.zero), v3.tensor1.make(v3.scalar.zero,extent.y, v3.scalar.zero))
  }


}

object Rectangle2Over
{
  def fromMinMax[V[_,_ <: Nat],@specialized F : ClassTag : Field](min:V[F,_2], max:V[F,_2])(implicit evTag: ClassTag[V[F,_2]], ev : CanonicalEuclideanSpaceOverField[V,F,_2] , c: ConvertibleFromDouble[F], tensor1: Tensor1[F,V,_2]):Rectangle2Over[V,F] =
  {
    val t = (max - min)* 0.5D.as[F]
    Rectangle2Over(min + t, t)
  }

  def apply[V[_,_ <: Nat], @specialized F : ClassTag : Field](center:V[F,_2], extent:V[F,_2])(implicit evTag: ClassTag[V[F,_2]], ev : CanonicalEuclideanSpaceOverField[V,F,_2], tensor1: Tensor1[F,V,_2]) = new Rectangle2Over[V,F](center, extent)
}

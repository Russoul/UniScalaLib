package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses._
import russoul.lib.common.TypeClasses.Field
import russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import russoul.lib.common.{immutable, tbsp}
import russoul.lib.common.math.geometry.simple.general.{CenteredShape}
import russoul.lib.common.utils.Arr
import russoul.lib.common.Implicits._
import shapeless.Nat
import shapeless.Nat._
import russoul.lib.common.Abstraction._
import russoul.lib.common._

import scala.reflect.ClassTag


import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 11.03.17.
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Rectangle2Over[V[_,_ <: Nat], @tbsp F]private(override val center:V[F,_2],val extent:V[F,_2]) extends CenteredShape[V,F,_2]{

  override def translate(v: V[F,_2])(implicit ev1 : CES[V,F,_2], ev2: T1[F,V,_2], ev3 : Field[F]): Rectangle2Over[V,F] = {
    new Rectangle2Over(center + v, extent)
  }

  def genVertices()(implicit evTag: ClassTag[V[F,_2]],ev1 : CES[V,F,_2], ev2: T1[F,V,_2], ev3 : Field[F]): Array[V[F,_2]] = Array[V[F,_2]](center - extent, center + makeVector(_2, extent.x, -extent.y), center + extent, center + makeVector(_2, -extent.x, extent.y))

  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F)(implicit ev1 : CES[V,F,_2], ev2: T1[F,V,_2], ev3 : Field[F]): Rectangle2Over[V,F] =
  {
    new Rectangle2Over(center, extent * scalar)
  }

  def scaleAroundBasis(scalar:F)(implicit ev1 : CES[V,F,_2], ev2: T1[F,V,_2], ev3 : Field[F]):Rectangle2Over[V,F] =
  {
    new Rectangle2Over(center * scalar, extent * scalar)
  }

  override def toString: String =
  {
    "Rectangle2(center = "+center.toString() + ";extent = " + extent.toString() + ")"
  }


  def toRectangleParallelToZ(zLevel:F)(implicit ev1 : CES[V,F,_2], ev2: T1[F,V,_2], field : Field[F], ev3: CES[V,F,_3]): RectangleOver[V,F] =
  {
    RectangleOver[V,F](makeVector(_3, center.x, center.y, zLevel), makeVector(_3, extent.x, field.zero, field.zero), makeVector(_3, field.zero,extent.y, field.zero))
  }


}

object Rectangle2Over
{
  def fromMinMax[V[_,_ <: Nat],@tbsp F](min:V[F,_2], max:V[F,_2])(implicit ev : CES[V,F,_2] , c: Con[F], tensor1: T1[F,V,_2]):Rectangle2Over[V,F] =
  {
    val t = (max - min)* 0.5D.as[F]
    Rectangle2Over(min + t, t)
  }

  def apply[V[_,_ <: Nat], @tbsp F](center:V[F,_2], extent:V[F,_2]) = new Rectangle2Over[V,F](center, extent)
}

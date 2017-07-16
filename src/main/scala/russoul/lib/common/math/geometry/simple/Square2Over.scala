package russoul.lib.common.math.geometry.simple

import russoul.lib.common.Abstraction.{CES, T1}
import russoul.lib.common.{immutable, tbsp}
import russoul.lib.common.math.geometry.simple.general.{CenteredShape, GeometricShape}
import russoul.lib.common.utils.Arr

import scala.reflect.ClassTag
import russoul.lib.common.Implicits._
import russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import shapeless.Nat
import shapeless.Nat._

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
@immutable class Square2Over[V[_,_ <: Nat],@tbsp F]private(override val center:V[F,_2], val extent:F) extends CenteredShape[V,F, _2]{




  override def translate(v: V[F, _2])(implicit ev1: CES[V, F, _2], ev2: T1[F, V, _2], ev3: Field[F]): Square2Over[V,F] = {
    new Square2Over(center + v, extent)
  }

  override def scaleAroundBasis(factor: F)(implicit ev1: CES[V, F, _2], ev2: T1[F, V, _2], ev3: Field[F]): Square2Over[V,F] = {
    new Square2Over(center * factor, extent * factor)
  }



  def genVertices()(implicit classtag: ClassTag[V[F,_2]], ev: CES[V, F, _2], ev2: T1[F, V, _2], ev3: Field[F]): Array[V[F,_2]] = Array(center - ev.tensor1.make(extent,extent), center + ev.tensor1.make(extent, -extent), center + ev.tensor1.make(extent,extent), center + ev.tensor1.make(-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F)(implicit ev1: CES[V, F, _2], ev2: T1[F, V, _2], ev3: Field[F]): Square2Over[V,F] =
  {
    new Square2Over(center, extent * scalar)
  }

  def toRectangle2()(implicit ev: CES[V, F, _2], ev2: T1[F, V, _2], ev3: Field[F]):Rectangle2Over[V,F] =
  {
    Rectangle2Over[V,F](center, ev.tensor1.make(extent, extent))
  }

  override def toString: String =
  {
    "Square2(center = "+center.toString() + ";extent = " + extent + ")"
  }

}

object Square2Over{
  def apply[V[_,_ <: Nat],@tbsp F](center:V[F,_2], extent:F) = new Square2Over[V,F](center, extent)
}

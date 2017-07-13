package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.{immutable, tbsp}
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.utils.Arr

import scala.reflect.ClassTag
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import shapeless.Nat
import shapeless.Nat._

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
@immutable class Square2Over[V[_,_ <: Nat],@tbsp F : ClassTag : Field]private(val center:V[F,_2], val extent:F)(implicit evTag: ClassTag[V[F,_2]],ev: CanonicalEuclideanSpaceOverField[V,F,_2], tensor1: Tensor1[F,V,_2])  extends CenteredShape2[V[F,_2],F]{



  override def translate(v: V[F,_2]): Square2Over[V,F] = {
    new Square2Over(center + v, extent)
  }

  override def scaleAroundBasis(factor: F): Square2Over[V,F] = {
    new Square2Over(center * factor, extent * factor)
  }

  def genVertices(): Array[V[F,_2]] = Array(center - ev.tensor1.make(extent,extent), center + ev.tensor1.make(extent, -extent), center + ev.tensor1.make(extent,extent), center + ev.tensor1.make(-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F): Square2Over[V,F] =
  {
    new Square2Over(center, extent * scalar)
  }

  def toRectangle2():Rectangle2Over[V,F] =
  {
    Rectangle2Over[V,F](center, ev.tensor1.make(extent, extent))
  }

  override def toString: String =
  {
    "Square2(center = "+center.toString() + ";extent = " + extent + ")"
  }

}

object Square2Over{
  def apply[V[_,_ <: Nat],@tbsp F : ClassTag : Field](center:V[F,_2], extent:F)(implicit evTag: ClassTag[V[F,_2]], ev: CanonicalEuclideanSpaceOverField[V,F,_2], tensor1: Tensor1[F,V,_2]) = new Square2Over[V,F](center, extent)
}

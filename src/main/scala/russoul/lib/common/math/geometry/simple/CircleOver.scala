package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import russoul.lib.common.immutable
import russoul.lib.common.Implicits._
import shapeless.Nat._

import russoul.lib.common._
import shapeless.Nat
import Abstraction._
import russoul.lib.common.math.geometry.simple.general.CenteredShape

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class CircleOver[V[_,_ <: Nat] ,@tbsp F]private(override val center:V[F,_2],val rad:F) extends CenteredShape[V,F,_2] {


  override def translate(v: V[F,_2])(implicit ev1 : CES[V,F,_2], tensor1: T1[F,V,_2], field: Field[F]): CircleOver[V,F] = {
    new CircleOver(center + v, rad)
  }

  override def scale(scalar:F)(implicit ev1 : CES[V,F,_2], tensor1: T1[F,V,_2], field: Field[F]): CircleOver[V,F] = {
    new CircleOver(center, rad * scalar)
  }

  override def scaleAroundBasis(scalar:F)(implicit ev1 : CES[V,F,_2], tensor1: T1[F,V,_2], field: Field[F]): CircleOver[V,F] = {
    new CircleOver(center * scalar, rad * scalar)
  }

  def inscribedInRectangle2()(implicit ev1 : CES[V,F,_2], tensor1: T1[F,V,_2], field: Field[F]): Rectangle2Over[V,F] = {
    Rectangle2Over[V,F](center, makeVector(_2, rad,rad))
  }

  override def toString(): String =
  {
    "Circle( center = " + center + "; radius = " + rad + " )"
  }
}

object CircleOver{
  def apply[V[_,_ <: Nat],@tbsp F](center:V[F,_2], rad:F) = new CircleOver[V,F](center, rad)
}

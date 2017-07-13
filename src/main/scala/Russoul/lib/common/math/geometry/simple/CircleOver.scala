package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor, Tensor1}
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.Implicits._
import shapeless.Nat._

import scala.reflect.ClassTag
import Russoul.lib.common._
import shapeless.Nat

/**
  * Created by russoul on 23.04.17.
  */
@immutable class CircleOver[V[_,_ <: Nat] ,@tbsp F : ClassTag : Field]private(val center:V[F,_2],val rad:F)(implicit evTag: ClassTag[V[F,_2]], ev : CanonicalEuclideanSpaceOverField[V,F,_2], tensor1: Tensor1[F,V,_2]) extends CenteredShape2[V[F,_2],F] {


  override def translate(v: V[F,_2]): CircleOver[V,F] = {
    new CircleOver(center + v, rad)
  }

  override def scale(scalar:F): CircleOver[V,F] = {
    new CircleOver(center, rad * scalar)
  }

  override def scaleAroundBasis(scalar:F): CircleOver[V,F] = {
    new CircleOver(center * scalar, rad * scalar)
  }

  def inscribedInRectangle2(): Rectangle2Over[V,F] = {
    Rectangle2Over[V,F](center, makeVector(_2, rad,rad))
  }

  override def toString(): String =
  {
    "Circle( center = " + center + "; radius = " + rad + " )"
  }
}

object CircleOver{
  def apply[V[_,_ <: Nat],@tbsp F : ClassTag : Field](center:V[F,_2], rad:F)(implicit evTag: ClassTag[V[F,_2]], ev : CanonicalEuclideanSpaceOverField[V,F,_2], tensor1: Tensor1[F,V,_2]) = new CircleOver[V,F](center, rad)
}

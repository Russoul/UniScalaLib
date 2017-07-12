package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.{Real, Real3, immutable}
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField

import Russoul.lib.common.Implicits._
import shapeless.Nat._

/**
  * Created by Russoul on 23.07.2016.
  */
@immutable class TriangleOver[V[_,_], @specialized F] private(val p1:V[F,_3], val p2:V[F,_3], val p3:V[F,_3])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3]) extends Shape3[V[F,_3],F] {


  override def translate(v: V[F,_3]): TriangleOver[V,F] = {
    new TriangleOver(p1 + v, p2 + v, p3 + v)
  }

  override def toString: String = {
    "Triangle(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

object TriangleOver{
  @inline def apply[V[_,_], @specialized F](p1:V[F,_3], p2:V[F,_3], p3:V[F,_3])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_3]) = new TriangleOver[V,F](p1,p2,p3)
}

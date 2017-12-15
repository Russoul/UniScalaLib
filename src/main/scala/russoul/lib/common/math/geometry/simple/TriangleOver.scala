package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses._
import russoul.lib.common.{Real, Real3, immutable, tbsp}
import russoul.lib.common.math.geometry.simple.general.{GeometricShape}
import russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import russoul.lib.common.Implicits._
import shapeless.Nat
import shapeless.Nat._
import russoul.lib.common.Abstraction._

import scala.reflect.ClassTag


import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by Russoul on 23.07.2016.
  */
@immutable case class TriangleOver[V[_,_ <: Nat], @tbsp F] private(val p1:V[F,_3], val p2:V[F,_3], val p3:V[F,_3]) extends GeometricShape[V,F,_3] {



  override def translate(v: V[F,_3])(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3: Field[F]): TriangleOver[V,F] = {
    new TriangleOver(p1 + v, p2 + v, p3 + v)
  }


  override def scaleAroundBasis(factor: F)(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3: Field[F]): TriangleOver[V, F] = {
    new TriangleOver(p1 * factor, p2 * factor, p3 * factor)
  }

  override def toString: String = {
    "Triangle(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

object TriangleOver{
  @inline def apply[V[_,_ <: Nat], @tbsp F](p1:V[F,_3], p2:V[F,_3], p3:V[F,_3]) = new TriangleOver[V,F](p1,p2,p3)
}

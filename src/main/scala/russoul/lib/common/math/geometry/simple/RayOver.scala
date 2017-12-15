package russoul.lib.common.math.geometry.simple

import russoul.lib.common._
import russoul.lib.common.{immutable, tbsp}
import russoul.lib.common.Implicits._
import russoul.lib.common.TypeClasses.{Field}
import shapeless.Nat
import shapeless.Nat._
import Abstraction._
import russoul.lib.common.math.geometry.simple.general.GeometricShape

import spire.algebra._
import spire.math._
import spire.implicits._

@immutable case class RayOver[V[_,_ <: Nat],@tbsp F]private (val start: V[F,_3],val dir: V[F,_3]) extends GeometricShape[V,F,_3] {

  override def translate(v: V[F,_3])(implicit ev1: CES[V,F,_3], ev2:T1[F,V,_3], ev3: Field[F]): RayOver[V,F] = {
    new RayOver(start + v, dir)
  }


  override def scaleAroundBasis(factor: F)(implicit ev1: CES[V, F, _3], ev2: T1[F, V, _3], ev3: Field[F]): RayOver[V, F] = {
    new RayOver(start * factor, dir)
  }

  override def toString(): String = {
    "Ray(start = " + start + ";dir = " + dir + ")"

  }
}

object RayOver
{

  def apply[V[_,_ <: Nat],@tbsp F](pos: V[F,_3], look: V[F,_3], zNear: F, zFar: F)(implicit ev: CES[V,F,_3], tensor1:T1[F,V,_3]): RayOver[V,F] =
  {
    new RayOver(pos + look * zNear, look)
  }

  def apply[V[_,_ <: Nat],@tbsp F](start: V[F,_3], dir: V[F,_3]) = new RayOver[V,F](start, dir)
}

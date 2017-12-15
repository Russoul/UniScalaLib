package russoul.lib.common.math.geometry.simple

import russoul.lib.common._
import russoul.lib.common.Implicits._
import russoul.lib.common.TypeClasses.Field
import shapeless.Nat
import shapeless.Nat._
import Abstraction._
import russoul.lib.common.math.geometry.simple.general.GeometricShape

import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class PlaneOver[V[_,_ <: Nat],@tbsp F]private(val point:V[F,_3],val normal:V[F,_3]) extends GeometricShape[V,F,_3]{


  override def translate(v: V[F,_3])(implicit ev1 : CES[V,F,_3], ev2: T1[F,V,_3], ev3: Field[F]): PlaneOver[V,F] = new PlaneOver[V,F](point, normal)


  override def scaleAroundBasis(factor: F)(implicit ev1: CES[V, F, _3], ev2: T1[F, V, _3], ev3: Field[F]): PlaneOver[V, F] = {
    new PlaneOver(point * factor, normal)
  }

  override def toString(): String = {
    "Plane( point = " + point + "; normal = " + normal + " )"
  }

}

object PlaneOver{
  def apply[V[_,_ <: Nat],@tbsp F](point:V[F,_3], normal:V[F,_3]) = new PlaneOver[V,F](point, normal)
}
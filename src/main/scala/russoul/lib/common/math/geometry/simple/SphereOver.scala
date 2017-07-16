package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import russoul.lib.common.math.geometry.simple.general.{CenteredShape, GeometricShape}
import russoul.lib.common.{immutable, tbsp}
import shapeless.Nat._
import russoul.lib.common.Implicits._
import shapeless.Nat
import russoul.lib.common.Abstraction._

/**
  * Created by Russoul on 21.04.2016.
  */
@immutable class SphereOver[V[_,_ <: Nat],@tbsp F]private (override val center:V[F,_3],val rad: F) extends CenteredShape[V,F,_3] {

  override def translate(v: V[F,_3])(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3: Field[F]): SphereOver[V,F] = {
    new SphereOver(center + v, rad)
  }


  override def scaleAroundBasis(factor: F)(implicit ev1: CES[V, F, _3], ev2: T1[F, V, _3], ev3: Field[F]): SphereOver[V, F] = {
    new SphereOver(center * factor, rad * factor)
  }

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F)(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3: Field[F]): SphereOver[V,F] = {
    new SphereOver(center, factor * rad)
  }

  override def toString: String = {
    "Sphere(center = " + center.toString() + ";radius = " + rad + " )"
  }
}

object SphereOver{
  def apply[V[_,_ <: Nat],@tbsp F](center:V[F,_3],rad: F) = new SphereOver[V,F](center, rad)
}

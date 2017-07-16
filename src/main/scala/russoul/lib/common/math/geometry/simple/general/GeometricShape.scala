package russoul.lib.common.math.geometry.simple.general

import russoul.lib.common.Abstraction.{CES, T1}
import russoul.lib.common.TypeClasses.Field
import russoul.lib.common.tbsp
import shapeless.Nat


/**
  * Created by russoul on 11.05.17.
  */
trait GeometricShape[V[_,_ <: Nat],@tbsp F, Dim <: Nat] {
  def translate(v:V[F,Dim])(implicit ev1: CES[V,F,Dim], ev2: T1[F,V,Dim], ev3: Field[F]): GeometricShape[V,F,Dim]
  def scaleAroundBasis(factor:F)(implicit ev1: CES[V,F,Dim], ev2: T1[F,V,Dim], ev3: Field[F]):GeometricShape[V,F,Dim]
}

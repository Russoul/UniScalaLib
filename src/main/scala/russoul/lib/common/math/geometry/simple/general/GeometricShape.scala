package russoul.lib.common.math.geometry.simple.general

import russoul.lib.common.math.algebra.Vec
import russoul.lib.common.tbsp
import shapeless.Nat
import spire.algebra.Field


/**
  * Created by russoul on 11.05.17.
  */
trait GeometricShape[@tbsp F, Dim <: Nat] {
  def translate(v:Vec[F,Dim])(implicit ev : Field[F]): GeometricShape[F,Dim]
  def scaleAroundBasis(factor:F)(implicit ev3: Field[F]):GeometricShape[F,Dim]
}

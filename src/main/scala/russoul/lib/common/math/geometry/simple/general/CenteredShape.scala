package russoul.lib.common.math.geometry.simple.general

import russoul.lib.common.math.algebra.Vec
import russoul.lib.common.tbsp
import shapeless.Nat
import spire.algebra.Field


/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape[@tbsp F, Dim <: Nat] extends GeometricShape[F,Dim]
{
  def center:Vec[F,Dim]

  /**
    *
    * @param factor
    * @return scaled around its center
    */
  def scale(factor:F)(implicit ev3: Field[F]):CenteredShape[F,Dim]
}

package russoul.lib.common.math.geometry.simple.general

import russoul.lib.common.Abstraction.{CES, T1}
import russoul.lib.common.TypeClasses.Field
import russoul.lib.common.tbsp
import shapeless.Nat


/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape[V[_,_ <: Nat], @tbsp F, Dim <: Nat] extends GeometricShape[V,F,Dim]
{
  def center:V[F,Dim]

  /**
    *
    * @param factor
    * @return scaled around its center
    */
  def scale(factor:F)(implicit ev1: CES[V,F,Dim], ev2: T1[F,V,Dim], ev3: Field[F]):CenteredShape[V,F,Dim]
}

package russoul.lib.common.math.geometry.simple.general

import russoul.lib.common.math.algebra.Vec
import russoul.lib.common.tbsp
import shapeless.Nat
import singleton.ops.XInt
import spire.algebra.Field

import scala.reflect.ClassTag


/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape[@tbsp F, Dim <: XInt] extends GeometricShape[F,Dim]
{
  def center:Vec[F,Dim]

  /**
    *
    * @param factor
    * @return scaled around its center
    */
  def scale(factor:F)(implicit field: Field[F], tag : ClassTag[F]):CenteredShape[F,Dim]
}

package russoul.lib.common.math.geometry.simple.general

import russoul.lib.common.math.algebra.Row
import russoul.lib.common.tbsp
import shapeless.Nat
import singleton.ops.XInt
import spire.algebra.Field
import russoul.lib.common._

import scala.reflect.ClassTag


/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape[@specialized(Float,Double,Int) F, Dim <: XInt] extends GeometricShape[F,Dim]
{
  def center:Row[F,Dim]

  /**
    *
    * @param factor
    * @return scaled around its center
    */
  def scale(factor:F)(implicit field: Field[F], tag : ClassTag[F]):CenteredShape[F,Dim]
}

package russoul.lib.common.math.geometry.simple.general

import russoul.lib.common.math.algebra.Row
import russoul.lib.common.{Row, tbsp}
import shapeless.Nat
import singleton.ops.XInt
import spire.algebra.Field

import scala.reflect.ClassTag


/**
  * Created by russoul on 11.05.17.
  */
trait GeometricShape[@tbsp F, Dim <: XInt] {
  def translate(v:Row[F,Dim])(implicit ev : Field[F], tag : ClassTag[F]): GeometricShape[F,Dim]
  def scaleAroundBasis(factor:F)(implicit ev3: Field[F], tag : ClassTag[F]):GeometricShape[F,Dim]
}

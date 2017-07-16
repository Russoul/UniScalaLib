package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import russoul.lib.common._
import russoul.lib.common.Implicits._
import russoul.lib.common.utils.Arr
import shapeless.Nat
import shapeless.Nat._
import Abstraction._
import russoul.lib.common.math.geometry.simple.general.CenteredShape

import scala.reflect.ClassTag

/**
  * Created by russoul on 01.07.2017.
  */
@immutable case class OBB2Over[V[_,_ <: Nat], @tbsp F]private(override val center: V[F,_2], val right: V[F,_2], val up: V[F,_2], val extentRight : F, val extentUp: F) extends CenteredShape[V,F,_2] {

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F)(implicit ev1 : CES[V,F, _2], tensor1:T1[F,V,_2], field: Field[F]): OBB2Over[V, F] = {
    new OBB2Over(center, right, up, extentRight * factor, extentUp * factor)
  }

  override def translate(v: V[F,_2])(implicit ev1 : CES[V,F, _2], tensor1:T1[F,V,_2], field: Field[F]): OBB2Over[V, F] = {
    new OBB2Over(center + v, right, up, extentRight, extentUp)
  }

  override def scaleAroundBasis(factor: F)(implicit ev1 : CES[V,F, _2], tensor1:T1[F,V,_2], field: Field[F]): OBB2Over[V, F] = {
    new OBB2Over(center * factor, right, up, extentRight * factor, extentUp * factor)
  }

  def genVertices()(implicit ev1 : CES[V,F, _2], tensor1:T1[F,V,_2], field: Field[F], tag: ClassTag[V[F,_2]]): Array[V[F,_2]] = Array[V[F,_2]](center - right * extentRight - up * extentUp, center + right * extentRight - up * extentUp, center + right * extentRight + up * extentUp, center - right * extentRight + up * extentUp)

  override def toString: String =
  {
    "OBB2(center = "+center.toString() + ";right = " + right.toString() + ";up = " + up.toString() + ";extentRight = " + extentRight + ";extentUp = " + extentUp + ")"
  }
}

object OBB2Over{
  def apply[V[_,_ <: Nat], @tbsp F](center: V[F,_2], right: V[F,_2], up: V[F,_2], extentRight : F, extentUp: F) = new OBB2Over[V,F](center, right, up, extentRight, extentUp)
}

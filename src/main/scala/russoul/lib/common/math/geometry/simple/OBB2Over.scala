package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field, Tensor1}
import russoul.lib.common.{immutable, tbsp}
import russoul.lib.common.math.geometry.simple.general.{CenteredShape2, Shape2}
import russoul.lib.common.Implicits._
import russoul.lib.common.utils.Arr
import shapeless.Nat
import shapeless.Nat._

import scala.reflect.ClassTag

/**
  * Created by russoul on 01.07.2017.
  */
@immutable class OBB2Over[V[_,_ <: Nat], @tbsp F : Field : ClassTag]private(val center: V[F,_2], val right: V[F,_2], val up: V[F,_2], val extentRight : F, val extentUp: F)(implicit evTag: ClassTag[V[F,_2]], ev : CanonicalEuclideanSpaceOverField[V,F, _2], tensor1:Tensor1[F,V,_2]) extends CenteredShape2[V[F,_2],F] {

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F): OBB2Over[V, F] = {
    new OBB2Over(center, right, up, extentRight * factor, extentUp * factor)
  }

  override def translate(v: V[F,_2]): OBB2Over[V, F] = {
    new OBB2Over(center + v, right, up, extentRight, extentUp)
  }

  override def scaleAroundBasis(factor: F): OBB2Over[V, F] = {
    new OBB2Over(center * factor, right, up, extentRight * factor, extentUp * factor)
  }

  def genVertices(): Array[V[F,_2]] = Array[V[F,_2]](center - right * extentRight - up * extentUp, center + right * extentRight - up * extentUp, center + right * extentRight + up * extentUp, center - right * extentRight + up * extentUp)

  override def toString: String =
  {
    "OBB2(center = "+center.toString() + ";right = " + right.toString() + ";up = " + up.toString() + ";extentRight = " + extentRight + ";extentUp = " + extentUp + ")"
  }
}

object OBB2Over{
  def apply[V[_,_ <: Nat], @tbsp F : Field : ClassTag](center: V[F,_2], right: V[F,_2], up: V[F,_2], extentRight : F, extentUp: F)(implicit evTag: ClassTag[V[F,_2]], ev : CanonicalEuclideanSpaceOverField[V,F, _2], tensor1:Tensor1[F,V,_2]) = new OBB2Over[V,F](center, right, up, extentRight, extentUp)
}

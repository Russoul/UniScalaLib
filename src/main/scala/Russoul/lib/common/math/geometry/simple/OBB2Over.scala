package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape2, Shape2}
import Russoul.lib.common.Implicits._
import Russoul.lib.common.utils.Arr
import shapeless.Nat
import shapeless.Nat._

import scala.reflect.ClassTag

/**
  * Created by russoul on 01.07.2017.
  */
@immutable class OBB2Over[V[_,_ <: Nat] : ClassTag, @specialized F : Field : ClassTag]private(val center: V[F,_2], val right: V[F,_2], val up: V[F,_2], val extentRight : F, val extentUp: F)(implicit ev : CanonicalEuclideanSpaceOverField[V,F, _2]) extends CenteredShape2[V[F,_2],F] {

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
  def apply[V[_,_ <: Nat] : ClassTag, @specialized F : Field : ClassTag](center: V[F,_2], right: V[F,_2], up: V[F,_2], extentRight : F, extentUp: F)(implicit ev : CanonicalEuclideanSpaceOverField[V,F, _2]) = new OBB2Over[V,F](center, right, up, extentRight, extentUp)
}

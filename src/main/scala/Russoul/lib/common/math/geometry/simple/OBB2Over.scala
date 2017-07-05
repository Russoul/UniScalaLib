package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape2, Shape2}
import Russoul.lib.common.Implicits._
import Russoul.lib.common.utils.Arr

import scala.reflect.ClassTag

/**
  * Created by russoul on 01.07.2017.
  */
@immutable case class OBB2Over[V : ClassTag, @specialized F : Field](center: V, right: V, up: V, extentRight : F, extentUp: F)(implicit ev : CanonicalEuclideanSpaceOverField[V,F]) extends CenteredShape2[V,F] {
  assert(ev.dimensions == 2)

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F): OBB2Over[V, F] = {
    OBB2Over(center, right, up, extentRight * factor, extentUp * factor)
  }

  override def translate(v: V): OBB2Over[V, F] = {
    OBB2Over(center + v, right, up, extentRight, extentUp)
  }

  override def scaleAroundBasis(factor: F): OBB2Over[V, F] = {
    OBB2Over(center * factor, right, up, extentRight * factor, extentUp * factor)
  }

  def genVertices(): Arr[V] = new Arr[V](center - right * extentRight - up * extentUp, center + right * extentRight - up * extentUp, center + right * extentRight + up * extentUp, center - right * extentRight + up * extentUp)

  override def toString: String =
  {
    "OBB2(center = "+center.toString() + ";right = " + right.toString() + ";up = " + up.toString() + ";extentRight = " + extentRight + ";extentUp = " + extentUp + ")"
  }
}

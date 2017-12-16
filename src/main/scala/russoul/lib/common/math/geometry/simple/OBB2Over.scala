package russoul.lib.common.math.geometry.simple

import russoul.lib.common.utils.Arr
import russoul.lib.common.math.geometry.simple.general.CenteredShape

import scala.reflect.ClassTag

import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 01.07.2017.
  */
@immutable case class OBB2Over[@tbsp F]private(override val center: Vec2[F], val right: Vec2[F], val up: Vec2[F], val extentRight : F, val extentUp: F) extends CenteredShape[F,_2] {

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F)(implicit field: Field[F]): OBB2Over[F] = {
    new OBB2Over(center, right, up, extentRight * factor, extentUp * factor)
  }

  override def translate(v: Vec2[F])(implicit field: Field[F]): OBB2Over[F] = {
    new OBB2Over(center + v, right, up, extentRight, extentUp)
  }

  override def scaleAroundBasis(factor: F)(implicit field: Field[F]): OBB2Over[F] = {
    new OBB2Over(center * factor, right, up, extentRight * factor, extentUp * factor)
  }

  def genVertices()(implicit field: Field[F], tag: ClassTag[Vec2[F]]): Array[Vec2[F]] = Array[Vec2[F]](center - right * extentRight - up * extentUp, center + right * extentRight - up * extentUp, center + right * extentRight + up * extentUp, center - right * extentRight + up * extentUp)

  override def toString: String =
  {
    "OBB2(center = "+center.toString() + ";right = " + right.toString() + ";up = " + up.toString() + ";extentRight = " + extentRight + ";extentUp = " + extentUp + ")"
  }
}

object OBB2Over{
  def apply[@tbsp F](center: Vec2[F], right: Vec2[F], up: Vec2[F], extentRight : F, extentUp: F) = new OBB2Over[F](center, right, up, extentRight, extentUp)
}

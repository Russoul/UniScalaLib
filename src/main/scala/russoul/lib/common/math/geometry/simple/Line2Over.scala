package russoul.lib.common.math.geometry.simple

import russoul.lib.common.math.geometry.simple.general.GeometricShape


import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._
/**
  * Created by russoul on 23.04.17.
  */
@immutable case class Line2Over[@tbsp F]private(val start:Vec2[F], val end:Vec2[F]) extends GeometricShape[F,_2] {


  def genDir()(implicit field: Field[F]): Vec2[F] = (end - start).normalize()


  override def translate(v: Vec2[F])(implicitfield: Field[F]): Line2Over[F] = {
    new Line2Over(start + v, end + v)
  }

  override def toString(): String = {
    "Line2(start = " + start + ";end = " + end + " )"

  }

  def scaleAroundBasis(scalar:F)(implicit field: Field[F]): Line2Over[F] =
  {
    new Line2Over(start * scalar, end * scalar)
  }

}

object Line2Over{
  def apply[@tbsp F](start:Vec2[F], end:Vec2[F]) = new Line2Over[F](start, end)
}
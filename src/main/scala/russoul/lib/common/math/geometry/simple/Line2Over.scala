package russoul.lib.common.math.geometry.simple

import russoul.lib.common.math.geometry.simple.general.GeometricShape
import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

import scala.reflect.ClassTag
/**
  * Created by russoul on 23.04.17.
  */
@immutable class Line2Over[@specialized(Float,Double,Int) F](val start:Vec2[F], val end:Vec2[F]) extends GeometricShape[F,_2] {


  def genDir()(implicit field: Field[F], classTag: ClassTag[F], nroot : NRoot[F]): Vec2[F] = (end - start).normalize


  override def translate(v: Vec2[F])(implicit field: Field[F], classTag: ClassTag[F]): Line2Over[F] = {
    new Line2Over(start + v, end + v)
  }

  override def toString(): String = {
    "Line2(start = " + start + ";end = " + end + " )"

  }

  def scaleAroundBasis(scalar:F)(implicit field: Field[F], classTag: ClassTag[F]): Line2Over[F] =
  {
    new Line2Over(start :* scalar, end :* scalar)
  }

}

object Line2Over{
  def apply[@specialized(Float,Double,Int) F](start:Vec2[F], end:Vec2[F]) = new Line2Over[F](start, end)
}
package russoul.lib.common.math.geometry.simple

import russoul.lib.common.{immutable, tbsp}
import russoul.lib.common.math.geometry.simple.general.{CenteredShape, GeometricShape}
import russoul.lib.common.utils.Arr

import scala.reflect.ClassTag


import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Square2Over[@specialized(Float,Double,Int) F](override val center:Vec2[F], val extent:F) extends CenteredShape[F, _2]{




  override def translate(v: Vec2[F])(implicit ev3 : Field[F], tag : ClassTag[F]): Square2Over[F] = {
    new Square2Over(center + v, extent)
  }

  override def scaleAroundBasis(factor: F)(implicit ev3: Field[F], tag : ClassTag[F]): Square2Over[F] = {
    new Square2Over(center :* factor, extent * factor)
  }



  def genVertices()(implicit classtag: ClassTag[F], ev3: Field[F]): Array[Vec2[F]] = Array(center - Vec2[F](extent,extent), center + Vec2[F](extent, -extent), center + Vec2[F](extent,extent), center + Vec2[F](-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F)(implicit ev3: Field[F], tag : ClassTag[F]): Square2Over[F] =
  {
    new Square2Over(center, extent * scalar)
  }

  def toRectangle2()(implicit ev3: Field[F], tag : ClassTag[F]):Rectangle2Over[F] =
  {
    Rectangle2Over[F](center, Vec2[F](extent, extent))
  }

  override def toString: String =
  {
    "Square2(center = "+center.toString() + ";extent = " + extent + ")"
  }

}

object Square2Over{
  def apply[@specialized(Float,Double,Int) F](center:Vec2[F], extent:F) = new Square2Over[F](center, extent)
}

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
@immutable case class Ray2Over[@specialized(Float,Double,Int) F](val start: Vec2[F],val dir: Vec2[F]) extends GeometricShape[F,_2] {

  override def translate(v: Vec2[F])(implicit field: Field[F], tag : ClassTag[F]): Ray2Over[F] = {
    new Ray2Over(start + v, dir)
  }

  override def toString(): String = {
    "Ray2(start = " + start + ";dir = " + dir + " )"
  }

  override def scaleAroundBasis(factor: F)(implicit field: Field[F], tag : ClassTag[F]): Ray2Over[F] = {
    new Ray2Over(start :* factor, dir)
  }
}

object Ray2Over{
  def apply[@specialized(Float,Double,Int) F](start: Vec2[F], dir: Vec2[F]) = new Ray2Over[F](start, dir)
}
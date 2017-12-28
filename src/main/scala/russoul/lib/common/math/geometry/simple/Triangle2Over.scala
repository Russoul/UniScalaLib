package russoul.lib.common.math.geometry.simple

import russoul.lib.common.math.geometry.simple.general.GeometricShape
import russoul.lib.common.{TypeClasses, immutable, tbsp}
import russoul.lib.common._
import Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

import scala.reflect.ClassTag

/**
  * Created by russoul on 17.07.2017.
  */
@immutable case class Triangle2Over[@specialized(Float,Double,Int) F] private(val p1:Vec2[F], val p2:Vec2[F], val p3:Vec2[F]) extends GeometricShape[F,_2] {

  

  override def translate(v: Vec2[F])(implicit ev : Field[F], tag : ClassTag[F]): Triangle2Over[F] = {
    new Triangle2Over(p1 + v, p2 + v, p3 + v)
  }

  override def scaleAroundBasis(factor: F)(implicit ev : Field[F], tag : ClassTag[F]): Triangle2Over[F] = {
    new Triangle2Over(p1 :* factor, p2 :* factor, p3 :* factor)
  }

  override def toString: String = {
    "Triangle2(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

object Triangle2Over{
  @inline def apply[@specialized(Float,Double,Int) F](p1:Vec2[F], p2:Vec2[F], p3:Vec2[F]) = new Triangle2Over[F](p1,p2,p3)
}

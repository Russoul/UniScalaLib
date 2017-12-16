package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses._
import russoul.lib.common._
import russoul.lib.common.math.geometry.simple.general.{GeometricShape}
import russoul.lib.common.Implicits._
import russoul.lib.common.Abstraction._

import scala.reflect.ClassTag

import Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by Russoul on 23.07.2016.
  */
@immutable case class TriangleOver[@tbsp F] private(val p1:Vec3[F], val p2:Vec3[F], val p3:Vec3[F]) extends GeometricShape[F, _3] {



  override def translate(v: Vec3[F])(implicit ev3: Field[F]): TriangleOver[F] = {
    new TriangleOver(p1 + v, p2 + v, p3 + v)
  }


  override def scaleAroundBasis(factor: F)(implicit ev3: Field[F]): TriangleOver[F] = {
    new TriangleOver(p1 :* factor, p2 :* factor, p3 :* factor)
  }

  override def toString: String = {
    "Triangle(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

object TriangleOver{
  @inline def apply[@tbsp F](p1:Vec3[F], p2:Vec3[F], p3:Vec3[F]) = new TriangleOver[F](p1,p2,p3)
}

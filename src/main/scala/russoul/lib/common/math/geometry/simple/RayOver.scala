package russoul.lib.common.math.geometry.simple

import russoul.lib.common._
import russoul.lib.common.math.geometry.simple.general.GeometricShape

import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

@immutable case class RayOver[@tbsp F]private (val start: Vec3[F],val dir: Vec3[F]) extends GeometricShape[F,_3] {

  override def translate(v: Vec3[F])(implicit ev3: Field[F]): RayOver[F] = {
    new RayOver(start + v, dir)
  }


  override def scaleAroundBasis(factor: F)(implicit ev3: Field[F]): RayOver[F] = {
    new RayOver(start :* factor, dir)
  }

  override def toString(): String = {
    "Ray(start = " + start + ";dir = " + dir + ")"

  }
}

object RayOver
{

  def apply[@tbsp F](pos: Vec3[F], look: Vec3[F], zNear: F, zFar: F)(implicit field: Field[F]): RayOver[F] =
  {
    new RayOver(pos + (look * zNear), look)
  }

  def apply[@tbsp F](start: Vec3[F], dir: Vec3[F]) = new RayOver[F](start, dir)
}

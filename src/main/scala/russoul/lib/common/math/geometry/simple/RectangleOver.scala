package russoul.lib.common.math.geometry.simple

import russoul.lib.common
import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra.Mat
import russoul.lib.common.math.geometry.simple.general.{CenteredShape}
import russoul.lib.common.utils.Arr

import scala.reflect.ClassTag


import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * @note extents must be orthogonal
  *
  *       ^^
  *  up   ||
  *       ||
  *       ||
  * right XX----->
  */


/**
  *
  * @param center
  * @param right fully scaled
  * @param up fully scaled
  * @tparam F
  */
@immutable case class RectangleOver[@tbsp F]private (override val center: Vec3[F], val right: Vec3[F], val up: Vec3[F]) extends CenteredShape[F,_3] {

  override def translate(v: Vec3[F])(implicit ev3 : Field[F], classTag: ClassTag[F]): RectangleOver[F] = {
    new RectangleOver(center + v, right, up)
  }

  /**
    *
    * @return right hand rule
    */
  def genNormal()(implicit ev3 : Field[F], classTag: ClassTag[F], root : NRoot[F]): Vec3[F] = (right ⨯ up).normalize


  def genVerticesClockwise()(implicit ev3 : Field[F], ev4: ClassTag[F]): Array[Vec3[F]] = Array[Vec3[F]](center + up - right, center + up + right, center - up + right, center - up - right)

  def genVertices()(implicit ev3 : Field[F], ev4: ClassTag[F]): Array[Vec3[F]] = Array[Vec3[F]](center - up - right, center - up + right, center + up + right, center + up - right)

  def scale(right:F, up:F)(implicit ev3 : Field[F], classTag: ClassTag[F]): RectangleOver[F] =
  {
    new RectangleOver(center, this.right :* right, this.up :* up)
  }


  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F)(implicit ev3 : Field[F], classTag: ClassTag[F]): RectangleOver[F] = {
    new RectangleOver(center, this.right :* factor, this.up :* factor)
  }

  def scaleAroundBasis(scale:F)(implicit ev3 : Field[F], classTag: ClassTag[F]): RectangleOver[F] =
  {
    new RectangleOver(center :* scale, this.right :* scale, this.up :* scale)
  }

  def scaleAroundBasisZConst(scale:F)(implicit ev3 : Field[F], tag : ClassTag[F]): RectangleOver[F] =
  {
    new RectangleOver(Vec3[F](center(0) * scale, center(1) * scale,center(2)), this.right :* scale, this.up :* scale)
  }


  override def toString: String =
  {
    "Rectangle( center = " + center.toString() + "; right = " + right.toString() + "; up = " + up.toString() + " )"
  }
}

object RectangleOver
{
  def fromMinMax2DParallelToZ[@tbsp F : ClassTag](min:Vec2[F], max:Vec2[F], z:F)(implicit field: Field[F]): RectangleOver[F] =
  {
    val t: Vec3[F] = Vec3[F](max(0), max(1) ,field.zero) - Vec3[F](min(0), min(1),field.zero)
    new RectangleOver(Vec3[F](min(0), min(1),z) + t :* field.fromDouble(0.5D), Vec3[F](t(0) / field.fromDouble(2D), field.zero, field.zero), Vec3[F](field.zero, t(1) / field.fromDouble(2D) ,field.zero))
  }

  def apply[@tbsp F](center: Vec3[F], right: Vec3[F], up: Vec3[F]) = new RectangleOver[F](center, right, up)

}

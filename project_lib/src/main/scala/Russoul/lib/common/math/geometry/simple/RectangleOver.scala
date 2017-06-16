package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Mat
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.utils.Arr

import scala.reflect.ClassTag
import Russoul.lib.common.Implicits._

/**
  * @note extents must be orthogonal
  *
  *       ^^
  *  up   ||
  *       ||
  *       ||
  * right XX----->
  */
@immutable case class RectangleOver[V : ClassTag, @specialized F : Field](center: V, right: V, up: V)(implicit ev : CanonicalEuclideanSpaceOverField[V,F] with CanonicalCrossProductOp[V]) extends CenteredShape3[V,F] {
  assert(ev.dimensions == 3)

  override def translate(v: V): RectangleOver[V,F] = {
    RectangleOver(center + v, right, up)
  }

  /**
    *
    * @return right hand rule
    */
  def genNormal(): V = ev.normalize(right ⨯ up)



  def genVerticesClockwise(): Arr[V] = Arr[V](center + up - right, center + up + right, center - up + right, center - up - right)

  def genVertices(): Arr[V] = Arr[V](center - up - right, center - up + right, center + up + right, center + up - right)

  def scale(right:F, up:F): RectangleOver[V,F] =
  {
    new RectangleOver(center, this.right * right, this.up * up)
  }


  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F): CenteredShape3[V, F] = {
    new RectangleOver(center, this.right * factor, this.up * factor)
  }

  def scaleAroundBasis(scale:F): RectangleOver[V,F] =
  {
    new RectangleOver(center*scale, this.right * scale, this.up * scale)
  }

  def scaleAroundBasisZConst(scale:F): RectangleOver[V,F] =
  {
    new RectangleOver(ev.create(center.x * scale, center.y * scale,center.z), this.right * scale, this.up * scale)
  }


  override def toString: String =
  {
    "Rectangle( center = " + center.toString() + "; right = " + right.toString() + "; up = " + up.toString() + " )"
  }
}

object RectangleOver
{
  def fromMinMax2DParallelToZ[V : ClassTag, @specialized F : Field, V2](min:V2, max:V2, z:F)(implicit ev2: CanonicalEuclideanSpaceOverField[V2,F], ev3: CanonicalEuclideanSpaceOverField[V,F] with CanonicalCrossProductOp[V], ev : ConvertibleFromDouble[F]): RectangleOver[V,F] =
  {
    val t:V = ev3.create(ev2.x(max), ev2.y(max),ev3.scalar.zero) - ev3.create(ev2.x(min), ev2.y(min),ev3.scalar.zero)
    new RectangleOver(ev3.create(ev2.x(min), ev2.y(min),z) + t*ev.fromDouble(0.5D), ev3.create(ev3.scalar.div(ev3.x(t), ev.fromDouble(2D)),ev3.scalar.zero,ev3.scalar.zero), ev3.create(ev3.scalar.zero,ev3.scalar.div(t.y , ev.fromDouble(2D)),ev3.scalar.zero))
  }
}

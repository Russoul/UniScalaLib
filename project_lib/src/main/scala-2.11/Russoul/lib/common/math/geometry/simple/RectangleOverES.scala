package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{ConvertibleFromDouble, EuclideanSpace2OverField, EuclideanSpace3OverField, EuclideanSpaceOverField, Field}
import Russoul.lib.common.TypeClasses.Field.Implicits._
import Russoul.lib.common.TypeClasses.EuclideanSpace3OverField.Implicits._
import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Mat
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.linear.{Vec2, Vec3, Mat4}
import Russoul.lib.common.utils.Arr


/**
  * @note extents must be orthogonal
  *
  *       ^^
  *  up   ||
  *       ||
  *       ||
  * right XX----->
  */
@immutable case class RectangleOverES[V, @specialized F](center: V, right: V, up: V)(implicit ev : EuclideanSpace3OverField[V,F]) extends CenteredShape3[V,F] {


  override def translate(v: V): RectangleOverES[V,F] = {
    RectangleOverES(center + v, right, up)
  }

  /**
    *
    * @return right hand rule
    */
  def genNormal()(implicit gram: Mat[F]): V = ev.normalize(right ⨯ up, gram)



  def genVerticesClockwise(): Arr[V] = Arr[V](center + up - right, center + up + right, center - up + right, center - up - right)

  def genVertices(): Arr[V] = Arr[V](center - up - right, center - up + right, center + up + right, center + up - right)

  def scale(right:F, up:F): RectangleOverES[V,F] =
  {
    new RectangleOverES(center, this.right * right, this.up * up)
  }


  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F): CenteredShape3[V, F] = {
    new RectangleOverES(center, this.right * factor, this.up * factor)
  }

  def scaleAroundBasis(scale:F): RectangleOverES[V,F] =
  {
    new RectangleOverES(center*scale, this.right * scale, this.up * scale)
  }

  def scaleAroundBasisZConst(scale:F): RectangleOverES[V,F] =
  {
    new RectangleOverES(ev.create(center.x*scale, center.y * scale,center.z), this.right * scale, this.up * scale)
  }


  override def toString: String =
  {
    "Rectangle( center = " + center.toString() + "; right = " + right.toString() + "; up = " + up.toString() + " )"
  }
}

object RectangleOverES
{
  def fromMinMax2DParallelToZ[V, @specialized F, V2](min:V2, max:V2, z:F)(implicit ev2: EuclideanSpace2OverField[V2,F], ev: EuclideanSpace3OverField[V,F] with ConvertibleFromDouble[F]): RectangleOverES[V,F] =
  {
    val t:V = ev.create(ev2.x(max), ev2.y(max),ev.zero) - ev.create(ev2.x(min), ev2.y(min),ev.zero)
    new RectangleOverES(ev.create(ev2.x(min), ev2.y(min),z) + t*ev.fromDouble(0.5D), ev.create(ev.div(ev.x(t), ev.fromDouble(2D)),ev.zero,ev.zero), ev.create(ev.zero,ev.div(t.y , ev.fromDouble(2D)),ev.zero))
  }
}

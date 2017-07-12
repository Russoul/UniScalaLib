package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common
import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Mat
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.utils.Arr
import Russoul.lib.common._
import scala.reflect.ClassTag
import Russoul.lib.common.Implicits._
import shapeless.Nat._

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
  * @param ev$1
  * @param ev
  * @param cross
  * @tparam V
  * @tparam F
  */
@immutable class RectangleOver[V[_,_] : ClassTag, @specialized F : Field]private (val center: V[F,_3], val right: V[F,_3], val up: V[F,_3])(implicit ev : CanonicalEuclideanSpaceOverField[V,F,_3] , cross: CrossProductOverCanonicalEuclideanSpaceOverField[V,F]) extends CenteredShape3[V[F,_3],F] {

  override def translate(v: V[F,_3]): RectangleOver[V,F] = {
    new RectangleOver(center + v, right, up)
  }

  /**
    *
    * @return right hand rule
    */
  def genNormal(): V[F,_3] = ev.normalize(right ⨯ up)



  def genVerticesClockwise(): Array[V[F,_3]] = Array[V[F,_3]](center + up - right, center + up + right, center - up + right, center - up - right)

  def genVertices(): Array[V[F,_3]] = Array[V[F,_3]](center - up - right, center - up + right, center + up + right, center + up - right)

  def scale(right:F, up:F): RectangleOver[V,F] =
  {
    new RectangleOver(center, this.right * right, this.up * up)
  }


  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F): CenteredShape3[V[F,_3], F] = {
    new RectangleOver(center, this.right * factor, this.up * factor)
  }

  def scaleAroundBasis(scale:F): RectangleOver[V,F] =
  {
    new RectangleOver(center*scale, this.right * scale, this.up * scale)
  }

  def scaleAroundBasisZConst(scale:F): RectangleOver[V,F] =
  {
    new RectangleOver(ev.staticContainer.factory.makeVector[_3](center.x * scale, center.y * scale,center.z), this.right * scale, this.up * scale)
  }


  override def toString: String =
  {
    "Rectangle( center = " + center.toString() + "; right = " + right.toString() + "; up = " + up.toString() + " )"
  }
}

object RectangleOver
{
  def fromMinMax2DParallelToZ[V[_,_] : ClassTag, @specialized F : Field](min:V[F,_2], max:V[F,_2], z:F)(implicit ev2: CanonicalEuclideanSpaceOverField[V,F,_2], ev3: CanonicalEuclideanSpaceOverField[V,F,_3], cross: CrossProductOverCanonicalEuclideanSpaceOverField[V,F], ev : ConvertibleFromDouble[F]): RectangleOver[V,F] =
  {
    val t: V[F,_3] = makeVector(_3, max.x, max.y ,ev3.scalar.zero) - makeVector(_3, min.x, min.y,ev3.scalar.zero)
    new RectangleOver(makeVector(_3, min.x, min.y,z) + t * 0.5D.as[F], makeVector(_3, t.x / 2D.as[F], ev3.scalar.zero, ev3.scalar.zero), makeVector(_3, ev3.scalar.zero, t.y / 2D.as[F] ,ev3.scalar.zero))
  }

  def apply[V[_,_] : ClassTag, @specialized F : Field](center: V[F,_3], right: V[F,_3], up: V[F,_3])(implicit ev : CanonicalEuclideanSpaceOverField[V,F,_3] , cross: CrossProductOverCanonicalEuclideanSpaceOverField[V,F]) = new RectangleOver[V,F](center, right, up)

}

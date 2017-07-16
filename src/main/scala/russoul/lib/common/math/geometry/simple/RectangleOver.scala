package russoul.lib.common.math.geometry.simple

import russoul.lib.common
import russoul.lib.common.TypeClasses._
import russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import russoul.lib.common.immutable
import russoul.lib.common.math.algebra.Mat
import russoul.lib.common.math.geometry.simple.general.{CenteredShape}
import russoul.lib.common.utils.Arr
import russoul.lib.common._

import scala.reflect.ClassTag
import russoul.lib.common.Implicits._
import shapeless.Nat
import shapeless.Nat._
import Abstraction._

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
  * @tparam V
  * @tparam F
  */
@immutable case class RectangleOver[V[_,_ <: Nat], @tbsp F]private (override val center: V[F,_3], val right: V[F,_3], val up: V[F,_3]) extends CenteredShape[V,F,_3] {

  override def translate(v: V[F,_3])(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3 : Field[F]): RectangleOver[V,F] = {
    new RectangleOver(center + v, right, up)
  }

  /**
    *
    * @return right hand rule
    */
  def genNormal()(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3 : Field[F], ev4: CP[V,F]): V[F,_3] = (right ⨯ up).normalize()


  def genVerticesClockwise()(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3 : Field[F], ev4: ClassTag[V[F,_3]]): Array[V[F,_3]] = Array[V[F,_3]](center + up - right, center + up + right, center - up + right, center - up - right)

  def genVertices()(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3 : Field[F], ev4: ClassTag[V[F,_3]]): Array[V[F,_3]] = Array[V[F,_3]](center - up - right, center - up + right, center + up + right, center + up - right)

  def scale(right:F, up:F)(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3 : Field[F]): RectangleOver[V,F] =
  {
    new RectangleOver(center, this.right * right, this.up * up)
  }


  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F)(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3 : Field[F]): RectangleOver[V, F] = {
    new RectangleOver(center, this.right * factor, this.up * factor)
  }

  def scaleAroundBasis(scale:F)(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3 : Field[F]): RectangleOver[V,F] =
  {
    new RectangleOver(center*scale, this.right * scale, this.up * scale)
  }

  def scaleAroundBasisZConst(scale:F)(implicit ev1: CES[V,F,_3], ev2: T1[F,V,_3], ev3 : Field[F]): RectangleOver[V,F] =
  {
    new RectangleOver(makeVector(_3, center.x * scale, center.y * scale,center.z), this.right * scale, this.up * scale)
  }


  override def toString: String =
  {
    "Rectangle( center = " + center.toString() + "; right = " + right.toString() + "; up = " + up.toString() + " )"
  }
}

object RectangleOver
{
  def fromMinMax2DParallelToZ[V[_,_ <: Nat], @tbsp F](min:V[F,_2], max:V[F,_2], z:F)(implicit evTag: ClassTag[V[F,_3]], ev2: CES[V,F,_2], ev3: CES[V,F,_3], cross: CP[V,F], ev : Con[F], tensor11: T1[F,V,_2],tensor12: T1[F,V,_3], field: Field[F]): RectangleOver[V,F] =
  {
    val t: V[F,_3] = makeVector(_3, max.x, max.y ,ev3.scalar.zero) - makeVector(_3, min.x, min.y,ev3.scalar.zero)
    new RectangleOver(makeVector(_3, min.x, min.y,z) + t * 0.5D.as[F], makeVector(_3, t.x / 2D.as[F], ev3.scalar.zero, ev3.scalar.zero), makeVector(_3, ev3.scalar.zero, t.y / 2D.as[F] ,ev3.scalar.zero))
  }

  def apply[V[_,_ <: Nat], @tbsp F](center: V[F,_3], right: V[F,_3], up: V[F,_3]) = new RectangleOver[V,F](center, right, up)

}

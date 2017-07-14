package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import russoul.lib.common.utils.Arr
import russoul.lib.common.immutable
import russoul.lib.common.Implicits._
import shapeless.Nat._
import russoul.lib.common._
import shapeless.Nat

import scala.reflect.ClassTag

@immutable class AABBOver[V[_,_ <: Nat], @tbsp F : Field : ClassTag]private (val center: V[F,_3],val extent: V[F,_3])(implicit evTag: ClassTag[V[F,_3]], ev : CanonicalEuclideanSpaceOverField[V,F,_3] , cross:  CrossProductOverCanonicalEuclideanSpaceOverField[V,F], tensor1:Tensor1[F,V,_3]) extends CenteredShape3[V[F,_3],F] {

  def genMin(): V[F,_3] = center - extent
  def genMax(): V[F,_3] = center + extent

  override def translate(v: V[F,_3]): AABBOver[V,F] =
  {
    new AABBOver(center + v, extent)
  }

  /**
    *
    * @return scaled version (around AABB's center point)
    */
  override def scale(s:F): AABBOver[V,F] =
  {
    new AABBOver(center, extent * s)
  }

  def genVertices(): Array[V[F,_3]] =
  {
    val a = new Array[V[F,_3]](8)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a(0) = makeVector(_3, center.x-sx, center.y-sy, center.z-sz)
    a(1) = makeVector(_3, center.x-sx, center.y-sy, center.z+sz)
    a(2) = makeVector(_3, center.x+sx, center.y-sy, center.z+sz)
    a(3) = makeVector(_3, center.x+sx, center.y-sy, center.z-sz)
    a(4) = makeVector(_3, center.x-sx, center.y+sy, center.z-sz)
    a(5) = makeVector(_3, center.x-sx, center.y+sy, center.z+sz)
    a(6) = makeVector(_3, center.x+sx, center.y+sy, center.z+sz)
    a(7) = makeVector(_3, center.x+sx, center.y+sy, center.z-sz)

    a
  }

  /**
    *
    *
    */
  def genRectangles(): Array[RectangleOver[V,F]] =
  {

    val a = new Array[RectangleOver[V,F]](6)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a(0) = RectangleOver[V,F](center + makeVector(_3,ev.scalar.zero, sy, ev.scalar.zero), makeVector(_3,sx, ev.scalar.zero,ev.scalar.zero), makeVector(_3,ev.scalar.zero,ev.scalar.zero,-sz))//top
    a(1) = RectangleOver[V,F](center + makeVector(_3,ev.scalar.zero, -sy, ev.scalar.zero), makeVector(_3,sx, ev.scalar.zero,ev.scalar.zero), makeVector(_3,ev.scalar.zero,ev.scalar.zero,sz))//bottom
    a(2) = RectangleOver[V,F](center + makeVector(_3,-sx, ev.scalar.zero, ev.scalar.zero), makeVector(_3,ev.scalar.zero, ev.scalar.zero,sz), makeVector(_3,ev.scalar.zero,sy,ev.scalar.zero))//left
    a(3) = RectangleOver[V,F](center + makeVector(_3,sx, ev.scalar.zero, ev.scalar.zero), makeVector(_3,ev.scalar.zero, ev.scalar.zero,-sz), makeVector(_3,ev.scalar.zero,sy,ev.scalar.zero))//right
    a(4) = RectangleOver[V,F](center + makeVector(_3,ev.scalar.zero, ev.scalar.zero, -sz), makeVector(_3,-sx, ev.scalar.zero,ev.scalar.zero), makeVector(_3,ev.scalar.zero,sy,ev.scalar.zero))//back
    a(5) = RectangleOver[V,F](center + makeVector(_3,ev.scalar.zero, ev.scalar.zero, sz), makeVector(_3,sx, ev.scalar.zero,ev.scalar.zero), makeVector(_3,ev.scalar.zero,sy,ev.scalar.zero))//front
    
    
    a
  }

  override def toString(): String =
  {
    "AABB(center = " + center + ";extent = " + extent + ")"

  }

}

object AABBOver
{
  def genFromMinMax[V[_,_ <: Nat],@tbsp F : Field : ClassTag](min:V[F,_3], max:V[F,_3])(implicit evTag: ClassTag[V[F,_3]], v: CanonicalEuclideanSpaceOverField[V,F,_3] , cross : CrossProductOverCanonicalEuclideanSpaceOverField[V,F], c: ConvertibleFromDouble[F], tensor1:Tensor1[F,V,_3]):AABBOver[V,F] =
  {
    val extent = (max-min) * 0.5D.as[F]
    val center = min + extent

    new AABBOver[V,F](center,extent)
  }

  def apply[V[_,_ <: Nat], @tbsp F : Field : ClassTag](center: V[F,_3], extent: V[F,_3])(implicit evTag: ClassTag[V[F,_3]],  ev : CanonicalEuclideanSpaceOverField[V,F,_3] , cross:  CrossProductOverCanonicalEuclideanSpaceOverField[V,F], tensor1:Tensor1[F,V,_3]) = new AABBOver[V,F](center, extent)
}

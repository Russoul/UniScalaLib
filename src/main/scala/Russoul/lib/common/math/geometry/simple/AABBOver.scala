package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{CanonicalCrossProductOp, CanonicalEuclideanSpaceOverField, ConvertibleFromDouble, Field}
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.utils.Arr
import Russoul.lib.common.immutable
import Russoul.lib.common.Implicits._

import scala.reflect.ClassTag

@immutable case class AABBOver[V : ClassTag, @specialized F : Field](center: V, extent: V)(implicit ev : CanonicalEuclideanSpaceOverField[V,F] with CanonicalCrossProductOp[V]) extends CenteredShape3[V,F] {
  assert(ev.dimensions == 3)

  def genMin(): V = center - extent
  def genMax(): V = center + extent

  override def translate(v: V): AABBOver[V,F] =
  {
    new AABBOver(center + v, extent)
  }

  /**
    *
    * @param s
    * @return scaled version (around AABB's center point)
    */
  override def scale(s:F): AABBOver[V,F] =
  {
    new AABBOver(center, extent * s)
  }

  def genVertices(): Arr[V] =
  {
    val a = Arr[V](8)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a += ev.create(center.x-sx, center.y-sy, center.z-sz)
    a += ev.create(center.x-sx, center.y-sy, center.z+sz)
    a += ev.create(center.x+sx, center.y-sy, center.z+sz)
    a += ev.create(center.x+sx, center.y-sy, center.z-sz)
    a += ev.create(center.x-sx, center.y+sy, center.z-sz)
    a += ev.create(center.x-sx, center.y+sy, center.z+sz)
    a += ev.create(center.x+sx, center.y+sy, center.z+sz)
    a += ev.create(center.x+sx, center.y+sy, center.z-sz)

    a
  }

  /**
    *
    *
    */
  def genRectangles(): Arr[RectangleOver[V,F]] =
  {

    val a = Arr[RectangleOver[V,F]](6)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a+= new RectangleOver[V,F](center + ev.create(ev.scalar.zero, sy, ev.scalar.zero), ev.create(sx, ev.scalar.zero,ev.scalar.zero), ev.create(ev.scalar.zero,ev.scalar.zero,-sz))//top
    a+= new RectangleOver[V,F](center + ev.create(ev.scalar.zero, -sy, ev.scalar.zero), ev.create(sx, ev.scalar.zero,ev.scalar.zero), ev.create(ev.scalar.zero,ev.scalar.zero,sz))//bottom
    a+= new RectangleOver[V,F](center + ev.create(-sx, ev.scalar.zero, ev.scalar.zero), ev.create(ev.scalar.zero, ev.scalar.zero,sz), ev.create(ev.scalar.zero,sy,ev.scalar.zero))//left
    a+= new RectangleOver[V,F](center + ev.create(sx, ev.scalar.zero, ev.scalar.zero), ev.create(ev.scalar.zero, ev.scalar.zero,-sz), ev.create(ev.scalar.zero,sy,ev.scalar.zero))//right
    a+= new RectangleOver[V,F](center + ev.create(ev.scalar.zero, ev.scalar.zero, -sz), ev.create(-sx, ev.scalar.zero,ev.scalar.zero), ev.create(ev.scalar.zero,sy,ev.scalar.zero))//back
    a+= new RectangleOver[V,F](center + ev.create(ev.scalar.zero, ev.scalar.zero, sz), ev.create(sx, ev.scalar.zero,ev.scalar.zero), ev.create(ev.scalar.zero,sy,ev.scalar.zero))//front
    a
  }

  override def toString(): String =
  {
    "AABB(center = " + center + ";extent = " + extent + ")"

  }

}

object AABBOver
{
  def genFromMinMax[V : ClassTag,@specialized F : Field](min:V, max:V)(implicit v: CanonicalEuclideanSpaceOverField[V,F] with CanonicalCrossProductOp[V], c: ConvertibleFromDouble[F]):AABBOver[V,F] =
  {
    val extent = (max-min) * c.fromDouble(0.5D)
    val center = min + extent

    new AABBOver[V,F](center,extent)
  }
}

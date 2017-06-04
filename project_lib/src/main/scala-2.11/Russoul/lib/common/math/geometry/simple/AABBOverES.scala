package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.EuclideanSpace3OverField.Implicits._
import Russoul.lib.common.TypeClasses.Field.Implicits._
import Russoul.lib.common.TypeClasses.{ConvertibleFromDouble, EuclideanSpace3OverField, Field}
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.linear.{Vec3, Mat4}
import Russoul.lib.common.utils.Arr
import Field.Implicits._
import Russoul.lib.common.immutable

@immutable case class AABBOverES[V, @specialized F](center: V, extent: V)(implicit ev : EuclideanSpace3OverField[V,F]) extends CenteredShape3[V,F]
{


  def genMin(): V = center - extent
  def genMax(): V = center + extent

  override def translate(v: V): AABBOverES[V,F] =
  {
    new AABBOverES(center + v, extent)
  }

  /**
    *
    * @param s
    * @return scaled version (around AABB's center point)
    */
  override def scale(s:F): AABBOverES[V,F] =
  {
    new AABBOverES(center, extent * s)
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
  def genRectangles(): Arr[RectangleOverES[V,F]] =
  {

    val a = Arr[RectangleOverES[V,F]](6)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a+= new RectangleOverES(center + ev.create(ev.zero, sy, ev.zero), ev.create(sx, ev.zero,ev.zero), ev.create(ev.zero,ev.zero,-sz))//top
    a+= new RectangleOverES(center + ev.create(ev.zero, -sy, ev.zero), ev.create(sx, ev.zero,ev.zero), ev.create(ev.zero,ev.zero,sz))//bottom
    a+= new RectangleOverES(center + ev.create(-sx, ev.zero, ev.zero), ev.create(ev.zero, ev.zero,sz), ev.create(ev.zero,sy,ev.zero))//left
    a+= new RectangleOverES(center + ev.create(sx, ev.zero, ev.zero), ev.create(ev.zero, ev.zero,-sz), ev.create(ev.zero,sy,ev.zero))//right
    a+= new RectangleOverES(center + ev.create(ev.zero, ev.zero, -sz), ev.create(-sx, ev.zero,ev.zero), ev.create(ev.zero,sy,ev.zero))//back
    a+= new RectangleOverES(center + ev.create(ev.zero, ev.zero, sz), ev.create(sx, ev.zero,ev.zero), ev.create(ev.zero,sy,ev.zero))//front
    a
  }

  override def toString(): String =
  {
    "AABB(center = " + center + ";extent = " + extent + ")"

  }

}

object AABBOverES
{
  def genFromMinMax[V,@specialized F](min:V, max:V)(implicit v: EuclideanSpace3OverField[V,F] with ConvertibleFromDouble[F]):AABBOverES[V,F] =
  {
    val extent = (max-min) * v.fromDouble(0.5D)
    val center = min + extent

    new AABBOverES(center,extent)
  }
}

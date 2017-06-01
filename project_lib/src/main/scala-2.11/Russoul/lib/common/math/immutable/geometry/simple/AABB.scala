package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.{mat4, Vec3}
import Russoul.lib.common.utils.Arr

import Field.Implicits._

@immutable case class AABB[A](center: Vec3[A], extent: Vec3[A])(implicit ev : Field[A]) extends CenteredShape3[A]
{


  def genMin(): Vec3[A] = center - extent
  def genMax(): Vec3[A] = center + extent

  override def translate(v: Vec3[A]): AABB[A] =
  {
    new AABB(center + v, extent)
  }

  /**
    *
    * @param s
    * @return scaled version (around AABB's center point)
    */
  override def scale(s:A): AABB[A] =
  {
    new AABB(center, extent * s)
  }

  def genVertices(): Arr[Vec3[A]] =
  {
    val a = Arr[Vec3[A]](8)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z

    a += Vec3(center.x-sx, center.y-sy, center.z-sz)
    a += Vec3(center.x-sx, center.y-sy, center.z+sz)
    a += Vec3(center.x+sx, center.y-sy, center.z+sz)
    a += Vec3(center.x+sx, center.y-sy, center.z-sz)
    a += Vec3(center.x-sx, center.y+sy, center.z-sz)
    a += Vec3(center.x-sx, center.y+sy, center.z+sz)
    a += Vec3(center.x+sx, center.y+sy, center.z+sz)
    a += Vec3(center.x+sx, center.y+sy, center.z-sz)

    a
  }

  /**
    *
    *
    */
  def genRectangles(): Arr[Rectangle[A]] =
  {

    val a = Arr[Rectangle[A]](6)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a+= new Rectangle(center + Vec3(ev.zero, sy, ev.zero), Vec3(sx, ev.zero,ev.zero), Vec3(ev.zero,ev.zero,-sz))//top
    a+= new Rectangle(center + Vec3(ev.zero, -sy, ev.zero), Vec3(sx, ev.zero,ev.zero), Vec3(ev.zero,ev.zero,sz))//bottom
    a+= new Rectangle(center + Vec3(-sx, ev.zero, ev.zero), Vec3(ev.zero, ev.zero,sz), Vec3(ev.zero,sy,ev.zero))//left
    a+= new Rectangle(center + Vec3(sx, ev.zero, ev.zero), Vec3(ev.zero, ev.zero,-sz), Vec3(ev.zero,sy,ev.zero))//right
    a+= new Rectangle(center + Vec3(ev.zero, ev.zero, -sz), Vec3(-sx, ev.zero,ev.zero), Vec3(ev.zero,sy,ev.zero))//back
    a+= new Rectangle(center + Vec3(ev.zero, ev.zero, sz), Vec3(sx, ev.zero,ev.zero), Vec3(ev.zero,sy,ev.zero))//front
    a
  }

  override def toString(): String =
  {
    "AABB(center = " + center + ";extent = " + extent + ")"

  }

}

object AABB
{
  def genFromMinMax[A : Field](min:Vec3[A], max:Vec3[A]):AABB[A] =
  {
    val extent = (max-min) * implicitly[Field[A]].fromDouble(0.5D)
    val center = min + extent

    new AABB(center,extent)
  }
}

package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.{mat4, vec3}
import Russoul.lib.common.utils.vector


@immutable case class AABB(center: vec3, extent: vec3) extends CenteredShape3
{


  def genMin(): vec3 = center - extent
  def genMax(): vec3 = center + extent

  override def translate(v: vec3): AABB =
  {
    new AABB(center + v, extent)
  }

  /**
    *
    * @param s
    * @return scaled version (around AABB's center point)
    */
  override def scale(s:Float): AABB =
  {
    new AABB(center, extent * s)
  }

  def genVertices(): vector[vec3] =
  {
    val a = vector[vec3](8)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z

    a += vec3(center.x-sx, center.y-sy, center.z-sz)
    a += vec3(center.x-sx, center.y-sy, center.z+sz)
    a += vec3(center.x+sx, center.y-sy, center.z+sz)
    a += vec3(center.x+sx, center.y-sy, center.z-sz)
    a += vec3(center.x-sx, center.y+sy, center.z-sz)
    a += vec3(center.x-sx, center.y+sy, center.z+sz)
    a += vec3(center.x+sx, center.y+sy, center.z+sz)
    a += vec3(center.x+sx, center.y+sy, center.z-sz)

    a
  }

  /**
    *
    *
    */
  def genRectangles(): vector[Rectangle] =
  {

    val a = vector[Rectangle](6)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a+= new Rectangle(center + vec3(0, sy, 0), vec3(sx, 0,0), vec3(0,0,-sz))//top
    a+= new Rectangle(center + vec3(0, -sy, 0), vec3(sx, 0,0), vec3(0,0,sz))//bottom
    a+= new Rectangle(center + vec3(-sx, 0, 0), vec3(0, 0,sz), vec3(0,sy,0))//left
    a+= new Rectangle(center + vec3(sx, 0, 0), vec3(0, 0,-sz), vec3(0,sy,0))//right
    a+= new Rectangle(center + vec3(0, 0, -sz), vec3(-sx, 0,0), vec3(0,sy,0))//back
    a+= new Rectangle(center + vec3(0, 0, sz), vec3(sx, 0,0), vec3(0,sy,0))//front
    a
  }

  override def toString(): String =
  {
    "AABB(center = " + center + ";extent = " + extent + ")"

  }

}

object AABB
{
  def genFromMinMax(min:vec3, max:vec3):AABB =
  {
    val extent = (max-min)*0.5F
    val center = min + extent

    new AABB(center,extent)
  }
}

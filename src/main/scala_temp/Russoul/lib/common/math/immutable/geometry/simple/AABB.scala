package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.math.linear.vec3
import Russoul.lib.common.utils.vector


class AABB(val center: vec3, val extent: vec3)
{

  def copy() = new AABB(center.copy(), extent.copy())


  def genMin() = center - extent
  def genMax() = center + extent

  def translate(v: vec3) =
  {
    new AABB(center + v, extent)
  }

  def scale(s:Float) =
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

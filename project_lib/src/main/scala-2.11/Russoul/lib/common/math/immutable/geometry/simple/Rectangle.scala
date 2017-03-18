package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.{mat4, vec2, vec3}
import Russoul.lib.common.utils.vector


/**
  * @note extents must be orthogonal
  *
  *       ^^
  *  up   ||
  *       ||
  *       ||
  * right XX----->
  */
class Rectangle(val center: vec3,val right: vec3,val up: vec3)
{

  /**
    *
    * @return right hand rule
    */
  def genNormal() = right.crossProduct(up).normalize()



  def genVerticesClockwise() = vector[vec3](center + up - right, center + up + right, center - up + right, center - up - right)

  def genVertices() = vector[vec3](center - up - right, center - up + right, center + up + right, center + up - right)

  def scaleAroundOrigin(right:Float, up:Float): Rectangle =
  {
    new Rectangle(center, this.right * right, this.up * up)
  }

  def scaleAroundBasis(scale:Float): Rectangle =
  {
    new Rectangle(center*scale, this.right * scale, this.up * scale)
  }

  def scaleAroundBasisZConst(scale:Float): Rectangle =
  {
    new Rectangle(vec3(center.x*scale, center.y * scale,center.z), this.right * scale, this.up * scale)
  }

  override def toString: String =
  {
    center.toString() + ";" + right.toString() + ";" + up.toString()
  }
}

object Rectangle
{
  def fromMinMax2DParallelToZ(min:vec2, max:vec2, z:Float): Rectangle =
  {
    val t = vec3(max,0) - vec3(min,0)
    new Rectangle(vec3(min,z) + t*0.5F, vec3(t.x/2,0,0), vec3(0,t.y/2,0))
  }
}

package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.Shape3
import Russoul.lib.common.math.immutable.linear.{mat4, vec2, vec3}
import Russoul.lib.common.utils.Vector


/**
  * @note extents must be orthogonal
  *
  *       ^^
  *  up   ||
  *       ||
  *       ||
  * right XX----->
  */
@immutable case class Rectangle(center: vec3, right: vec3, up: vec3) extends Shape3 {


  override def translate(v: vec3): Rectangle = {
    Rectangle(center + v, right, up)
  }

  /**
    *
    * @return right hand rule
    */
  def genNormal(): vec3 = right.crossProduct(up).normalize()



  def genVerticesClockwise(): Vector[vec3] = Vector[vec3](center + up - right, center + up + right, center - up + right, center - up - right)

  def genVertices(): Vector[vec3] = Vector[vec3](center - up - right, center - up + right, center + up + right, center + up - right)

  def scale(right:Float, up:Float): Rectangle =
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
    "Rectangle( center = " + center.toString() + "; right = " + right.toString() + "; up = " + up.toString() + " )"
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

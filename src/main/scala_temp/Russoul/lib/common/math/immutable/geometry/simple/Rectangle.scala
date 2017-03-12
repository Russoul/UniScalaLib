package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.vec3
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

  override def toString: String =
  {
    center.toString() + ";" + right.toString() + ";" + up.toString()
  }
}

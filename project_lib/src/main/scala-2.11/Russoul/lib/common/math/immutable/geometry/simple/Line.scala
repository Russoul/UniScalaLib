package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.math.immutable.linear.vec3

/**
  * Created by Russoul on 18.07.2016.
  */
class Line(val start:vec3, val end:vec3)
{
  def genDir() = (end - start).normalize()

  def genRay() = new Ray(start, genDir())
}

object Line
{
  def apply(pos: vec3, start: Float, end: Float, yaw: Float, pitch: Float): Line =
  {
    val alpha = -yaw
    val t = math.toRadians(90 - alpha).toFloat
    val cosT = math.cos(t).toFloat
    val sinT = math.sin(t).toFloat
    val t2 = math.toRadians(pitch)
    val sinT2 = math.sin(t2).toFloat
    val cosT2 = math.cos(t2).toFloat

    val k = vec3(cosT * cosT2, sinT2, -sinT * cosT2)

    val p1 = k * start + pos
    val p2 = k * end + pos

    new Line(p1, p2)
  }
}

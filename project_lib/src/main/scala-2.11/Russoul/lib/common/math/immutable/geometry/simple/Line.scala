package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._
import Russoul.lib.common.math.immutable.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.immutable.linear.{mat4, Vec3}

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class Line[A](start:Vec3[A], end:Vec3[A])(implicit ev: FieldLike[A])  extends Shape3[A]
{


  override def translate(v: Vec3[A]): Line[A] = {
    Line(start + v, end + v)
  }

  def genDir(): Vec3[A] = (end - start).normalize()

  def genRay() = new Ray(start, genDir())

  override def toString(): String = {
    "Line(start = " + start + ";end = " + end + " )"

  }
}

object Line
{
  def apply[A](pos: Vec3[A], start: A, end: A, yaw: A, pitch: A)(implicit ev: FieldLike[A]): Line[A] = {
    val alpha = -yaw
    val t = ev.toRadians(ev.fromDouble(90D) - alpha)
    val cosT = ev.cos(t)
    val sinT = ev.sin(t)
    val t2 = ev.toRadians(pitch)
    val sinT2 = ev.sin(t2)
    val cosT2 = ev.cos(t2)

    val k = Vec3(cosT * cosT2, sinT2, -sinT * cosT2)

    val p1 = k * start + pos
    val p2 = k * end + pos

    new Line(p1, p2)
  }
}

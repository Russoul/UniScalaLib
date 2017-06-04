package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{ConvertibleFromDouble, EuclideanSpace3OverField, Field}
import Russoul.lib.common.TypeClasses.Field.Implicits._
import Russoul.lib.common.TypeClasses.EuclideanSpace3OverField.Implicits._
import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Mat
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}
import Russoul.lib.common.math.linear.{Vec3, Mat4}

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class LineOverES[V, @specialized F](start:V, end:V)(implicit ev: EuclideanSpace3OverField[V,F])  extends Shape3[V,F]
{


  override def translate(v: V): LineOverES[V,F] = {
    LineOverES(start + v, end + v)
  }

  def genDir()(implicit gram : Mat[F]): V = (end - start).normalize()(gram)

  def genRay() = new RayOverES(start, genDir())

  override def toString(): String = {
    "Line(start = " + start + ";end = " + end + " )"

  }
}

object LineOverES
{
  def apply[V, @specialized F](pos: V, start: F, end: F, yaw: F, pitch: F)(implicit ev: EuclideanSpace3OverField[V,F] with ConvertibleFromDouble[F]): LineOverES[V,F] = {
    val alpha = -yaw
    val t = ev.toRadians(ev.fromDouble(90D) - alpha)
    val cosT = ev.cos(t)
    val sinT = ev.sin(t)
    val t2 = ev.toRadians(pitch)
    val sinT2 = ev.sin(t2)
    val cosT2 = ev.cos(t2)

    val k = ev.create(cosT * cosT2, sinT2, -sinT * cosT2)

    val p1 = k * start + pos
    val p2 = k * end + pos

    new LineOverES(p1, p2)
  }
}

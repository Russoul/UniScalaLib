package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.Implicits._
import Russoul.lib.common.immutable
import Russoul.lib.common.math.algebra.Mat
import Russoul.lib.common.math.geometry.simple.general.{CenteredShape3, Shape3}

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class LineOver[V, @specialized F : Field](start:V, end:V)(implicit ev: CanonicalEuclideanSpaceOverField[V,F])  extends Shape3[V,F] {
  assert(ev.dimensions == 3)

  override def translate(v: V): LineOver[V,F] = {
    LineOver(start + v, end + v)
  }

  def genDir(): V = (end - start).normalize()

  def genRay() = new RayOver(start, genDir())

  override def toString(): String = {
    "Line(start = " + start + ";end = " + end + " )"

  }
}

object LineOver
{
  def apply[V, @specialized F : Field](pos: V, start: F, end: F, yaw: F, pitch: F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F], c: ConvertibleFromDouble[F]): LineOver[V,F] = {
    val alpha = -yaw
    val t = ev.scalar.toRadians(c.fromDouble(90D) - alpha)
    val cosT = ev.scalar.cos(t)
    val sinT = ev.scalar.sin(t)
    val t2 = ev.scalar.toRadians(pitch)
    val sinT2 = ev.scalar.sin(t2)
    val cosT2 = ev.scalar.cos(t2)

    val k = ev.create(cosT * cosT2, sinT2, -sinT * cosT2)

    val p1 = k * start + pos
    val p2 = k * end + pos

    new LineOver(p1, p2)
  }
}

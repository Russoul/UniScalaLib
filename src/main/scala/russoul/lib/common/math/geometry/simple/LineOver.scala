package russoul.lib.common.math.geometry.simple

import russoul.lib.common.math.geometry.simple.general.GeometricShape
import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

import scala.reflect.ClassTag



/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class LineOver[@specialized(Float,Double,Int) F](val start:Vec3[F], val end:Vec3[F]) extends GeometricShape[F,_3] {

  override def translate(v: Vec3[F])(implicit field: Field[F], classTag: ClassTag[F]): LineOver[F] = {
    new LineOver(start + v, end + v)
  }

  def genDir()(implicit field: Field[F], classTag: ClassTag[F], nroot : NRoot[F]): Vec3[F] = (end - start).normalize

  def genRay()(implicit field: Field[F], classTag: ClassTag[F], nroot : NRoot[F]) = RayOver[F](start, genDir())


  override def scaleAroundBasis(factor: F)(implicit ev3: Field[F], classTag: ClassTag[F]): LineOver[F] = {
    new LineOver[F](start * factor, end * factor)
  }

  override def toString(): String = {
    "Line(start = " + start + ";end = " + end + " )"

  }
}

object LineOver
{
  def apply[@specialized(Float,Double,Int) F](pos: Vec3[F], start: F, end: F, yaw: F, pitch: F)(implicit field: Field[F], trig: Trig[F], classTag: ClassTag[F], nroot : NRoot[F]): LineOver[F] = {
    val alpha = -yaw
    val t = trig.toRadians(field.fromDouble(90D) - alpha)
    val cosT = trig.cos(t)
    val sinT = trig.sin(t)
    val t2 = trig.toRadians(pitch)
    val sinT2 = trig.sin(t2)
    val cosT2 = trig.cos(t2)

    val k = Vec3[F](cosT * cosT2, sinT2, -sinT * cosT2)

    val p1 = (k :* start) + pos
    val p2 = (k :* end) + pos

    new LineOver(p1, p2)
  }

  def apply[@specialized(Float,Double,Int) F](start:Vec3[F], end:Vec3[F]) = new LineOver[F](start, end)
}

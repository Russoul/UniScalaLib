package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses._
import russoul.lib.common.Implicits._
import russoul.lib.common.immutable
import shapeless.Nat._
import russoul.lib.common._
import shapeless.Nat
import Abstraction._
import russoul.lib.common.math.geometry.simple.general.GeometricShape

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class LineOver[V[_,_ <: Nat], @tbsp F]private(val start:V[F,_3], val end:V[F,_3]) extends GeometricShape[V,F,_3] {

  override def translate(v: V[F,_3])(implicit ev1: CES[V,F,_3], tensor1: T1[F,V,_3], field: Field[F]): LineOver[V,F] = {
    new LineOver(start + v, end + v)
  }

  def genDir()(implicit ev1: CES[V,F,_3], tensor1: T1[F,V,_3], field: Field[F]): V[F,_3] = (end - start).normalize()

  def genRay()(implicit ev1: CES[V,F,_3], tensor1: T1[F,V,_3], field: Field[F]) = RayOver[V,F](start, genDir())


  override def scaleAroundBasis(factor: F)(implicit ev1: CES[V, F, _3], ev2: T1[F, V, _3], ev3: Field[F]): LineOver[V, F] = {
    new LineOver[V,F](start * factor, end * factor)
  }

  override def toString(): String = {
    "Line(start = " + start + ";end = " + end + " )"

  }
}

object LineOver
{
  def apply[V[_,_ <: Nat], @tbsp F](pos: V[F,_3], start: F, end: F, yaw: F, pitch: F)(implicit ev1: CES[V,F,_3], tensor1: T1[F,V,_3], field: Field[F], con: Con[F], trig: Trig[F]): LineOver[V,F] = {
    val alpha = -yaw
    val t = trig.toRadians(90D.as[F] - alpha)
    val cosT = trig.cos(t)
    val sinT = trig.sin(t)
    val t2 = trig.toRadians(pitch)
    val sinT2 = trig.sin(t2)
    val cosT2 = trig.cos(t2)

    val k = makeVector(_3, cosT * cosT2, sinT2, -sinT * cosT2)

    val p1 = k * start + pos
    val p2 = k * end + pos

    new LineOver(p1, p2)
  }

  def apply[V[_,_ <: Nat], @tbsp F](start:V[F,_3], end:V[F,_3]) = new LineOver[V,F](start, end)
}

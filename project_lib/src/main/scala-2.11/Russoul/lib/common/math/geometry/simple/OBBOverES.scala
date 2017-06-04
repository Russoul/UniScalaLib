package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{EuclideanSpace3OverField, Field}
import Russoul.lib.common.TypeClasses.Field.Implicits._
import Russoul.lib.common.TypeClasses.EuclideanSpace3OverField.Implicits._
import Russoul.lib.common.{Real, Real3, immutable}
import Russoul.lib.common.math.geometry.simple.general.CenteredShape3
import Russoul.lib.common.math.linear.{Mat4, Vec3}
import Russoul.lib.common.utils.Arr

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class OBBOverES[V,F](center:V, right:V, up:V, extentRight:F, extentUp:F, extentLook:F)(implicit ev: EuclideanSpace3OverField[V,F])  extends CenteredShape3[V,F]
{

  private def this(aabb :AABBOverES[V,F])
  {
    this(aabb.center, ev.create(ev.one,ev.zero,ev.zero), ev.create(ev.zero,ev.one,ev.zero), aabb.extent.x, aabb.extent.y, aabb.extent.z)
  }

  def genMax(): V =
  {
    val fe = (right ⨯ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    center + ue + re + fe
  }

  def genMin(): V =
  {
    val fe = (right ⨯ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    center - ue - re - fe
  }

  def genRightLine(length:F = 1D): LineOverES[V,F] =
  {
    LineOverES(center, center + right * length)
  }

  def genUpLine(length:F = 1D): LineOverES[V,F] = {
    LineOverES(center, center + up * length)
  }

  def genLookLine(length:F = 1): LineOverES[V,F] = {
    LineOverES(center, center + (right ⨯ up) * length)
  }


  def translate(tr:V): OBBOverES[V,F] =
  {
    OBBOverES(center + tr, right, up, extentRight, extentUp, extentLook)
  }

  def scale(s:F): OBBOverES[V,F] =
  {
    OBBOverES(center, right, up, extentRight * s, extentUp * s, extentLook * s)
  }




  def genRectangles():Arr[RectangleOverES[V,F]] =
  {
    val out = Arr[RectangleOverES[V,F]](6)

    val fe = (right ⨯ up) * extentLook
    val ue = up * extentUp
    val re = right * extentRight

    val t = center + ue
    val b = center - ue
    val l = center - re
    val r = center + re
    val ba = center - fe
    val f = center + fe

    out += new RectangleOverES(t, re, -fe)
    out += new RectangleOverES(b, re, fe)
    out += new RectangleOverES(l, fe, ue)
    out += new RectangleOverES(r, -fe, ue)
    out += new RectangleOverES(ba, -re, ue)
    out += new RectangleOverES(f, re, ue)

    out
  }

  def genVertices():Arr[V] =
  {
    val out = Arr[V](8)

    val fe = (right ⨯ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    out += center + ue + re + fe
    out += center + ue + re - fe
    out += center + ue - re - fe
    out += center + ue - re + fe

    out += center - ue + re + fe
    out += center - ue + re - fe
    out += center - ue - re - fe
    out += center - ue - re + fe

    out
  }

  def genVerticesCounterClockwise():Arr[V] =
  {
    val out = Arr[V](8)

    val l = (right ⨯ up)*extentLook
    val u =  up*extentUp
    val r = right*extentRight


    out += center - u - l - r
    out += center - u - l + r
    out += center - u + l + r
    out += center - u + l - r

    out += center + u - l - r
    out += center + u - l + r
    out += center + u + l + r
    out += center + u + l - r

    out
  }

  override def toString(): String =
  {
    "OBB(center = " + center + ";right = " + right +  ";up = " + up + ";extent = " + Vec3(extentRight,extentUp,extentLook) + ")"

  }
}

class OBBOverReal(center:Real3, right:Real3, up:Real3, extentRight:Real, extentUp:Real, extentLook:Real) extends OBBOverES[Real3, Real](center, right, up, extentRight, extentUp, extentLook){
  def rotateAroundRight(rad:Real): OBBOverES[Real3, Real] =
  {

    val mat = Mat4.matrixROTATIONRad(right, rad)
    new OBBOverES(center,right,up * mat,extentRight, extentUp, extentLook)
  }


  def rotateAroundUp(rad:Real): OBBOverES[Real3, Real] =
  {
    val mat = Mat4.matrixROTATIONRad(up, rad)
    new OBBOverES(center,right * mat,up,extentRight, extentUp, extentLook)
  }


  def rotateAroundLook(rad:Real):OBBOverES[Real3, Real] =
  {
    val mat = Mat4.matrixROTATIONRad(right ^ up, rad)
    new OBBOverES(center,right * mat,up * mat,extentRight, extentUp, extentLook)
  }
}

object OBBOverES
{
  def apply[V,@specialized F](aabb: AABBOverES[V,F])(implicit ev: EuclideanSpace3OverField[V,F]): OBBOverES[V,F] = new OBBOverES[V,F](aabb)

}



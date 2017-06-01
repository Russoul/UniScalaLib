package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.math.TypeClasses.Field.Implicits._
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape3
import Russoul.lib.common.math.immutable.linear.{mat4, Vec3}
import Russoul.lib.common.utils.Arr

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class OBB[A](center:Vec3[A], right:Vec3[A], up:Vec3[A], extentRight:A, extentUp:A, extentLook:A)(implicit ev: Field[A])  extends CenteredShape3[A]
{

  private def this(aabbInWorldSpace :AABB[A])
  {
    this(aabbInWorldSpace.center, Vec3(1,0,0), Vec3(0,1,0), aabbInWorldSpace.extent.x, aabbInWorldSpace.extent.y, aabbInWorldSpace.extent.z)
  }

  def genMax(): Vec3[A] =
  {
    val fe = (right ^ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    center + ue + re + fe
  }

  def genMin(): Vec3[A] =
  {
    val fe = (right ^ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    center - ue - re - fe
  }

  def genRightLine(length:A = 1D): Line[A] =
  {
    Line(center, center + right * length)
  }

  def genUpLine(length:A = 1D): Line[A] = {
    Line(center, center + up * length)
  }

  def genLookLine(length:A = 1): Line[A] = {
    Line(center, center + (right^up) * length)
  }


  def translate(tr:Vec3[A]): OBB[A] =
  {
    OBB(center + tr, right, up, extentRight, extentUp, extentLook)
  }

  def scale(s:A): OBB[A] =
  {
    OBB(center, right, up, extentRight * s, extentUp * s, extentLook * s)
  }


  def rotateAroundRight(rad:A): OBB[A] =
  {

    val mat = mat4.matrixROTATIONRad(right, rad)
    new OBB(center,right,up * mat,extentRight, extentUp, extentLook)
  }


  def rotateAroundUp(rad:A): OBB[A] =
  {
    val mat = mat4.matrixROTATIONRad(up, rad)
    new OBB(center,right * mat,up,extentRight, extentUp, extentLook)
  }


  def rotateAroundLook(rad:A):OBB[A] =
  {
    val mat = mat4.matrixROTATIONRad(right ^ up, rad)
    new OBB(center,right * mat,up * mat,extentRight, extentUp, extentLook)
  }

  def genRectangles():Arr[Rectangle[A]] =
  {
    val out = Arr[Rectangle[A]](6)

    val fe = (right ^ up) * extentLook
    val ue = up * extentUp
    val re = right * extentRight

    val t = center + ue
    val b = center - ue
    val l = center - re
    val r = center + re
    val ba = center - fe
    val f = center + fe

    out += new Rectangle(t, re, -fe)
    out += new Rectangle(b, re, fe)
    out += new Rectangle(l, fe, ue)
    out += new Rectangle(r, -fe, ue)
    out += new Rectangle(ba, -re, ue)
    out += new Rectangle(f, re, ue)

    out
  }

  def genVertices():Arr[Vec3[A]] =
  {
    val out = Arr[Vec3[A]](8)

    val fe = (right ^ up)*extentLook
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

  def genVerticesCounterClockwise():Arr[Vec3[A]] =
  {
    val out = Arr[Vec3[A]](8)

    val l = (right ^ up)*extentLook
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

object OBB
{
  def apply[A](aabb: AABB[A])(implicit ev: Field[A]): OBB[A] = new OBB[A](aabb)

}



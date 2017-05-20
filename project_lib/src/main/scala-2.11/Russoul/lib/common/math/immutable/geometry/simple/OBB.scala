package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.immutable.geometry.simple.general.CenteredShape3
import Russoul.lib.common.math.immutable.linear.{mat4, vec3}
import Russoul.lib.common.utils.Vector

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class OBB(center:vec3, right:vec3, up:vec3, extentRight:Float, extentUp:Float, extentLook:Float) extends CenteredShape3
{

  private def this(aabbInWorldSpace :AABB)
  {
    this(aabbInWorldSpace.center, vec3(1,0,0), vec3(0,1,0), aabbInWorldSpace.extent.x, aabbInWorldSpace.extent.y, aabbInWorldSpace.extent.z)
  }

  def genMax(): vec3 =
  {
    val fe = (right ^ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    center + ue + re + fe
  }

  def genMin(): vec3 =
  {
    val fe = (right ^ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    center - ue - re - fe
  }

  def genRightLine(length:Float = 1): Line =
  {
    Line(center, center + right * length)
  }

  def genUpLine(length:Float = 1): Line = {
    Line(center, center + up * length)
  }

  def genLookLine(length:Float = 1): Line = {
    Line(center, center + (right^up) * length)
  }


  def translate(tr:vec3): OBB =
  {
    OBB(center + tr, right, up, extentRight, extentUp, extentLook)
  }

  def scale(s:Float): OBB =
  {
    OBB(center, right, up, extentRight * s, extentUp * s, extentLook * s)
  }


  def rotateAroundRight(rad:Float): OBB =
  {

    val mat = mat4.matrixROTATIONRad(right, rad)
    new OBB(center,right,up * mat,extentRight, extentUp, extentLook)
  }


  def rotateAroundUp(rad:Float): Unit =
  {
    val mat = mat4.matrixROTATIONRad(up, rad)
    new OBB(center,right * mat,up,extentRight, extentUp, extentLook)
  }


  def rotateAroundLook(rad:Float):Unit =
  {
    val mat = mat4.matrixROTATIONRad(right ^ up, rad)
    new OBB(center,right * mat,up * mat,extentRight, extentUp, extentLook)
  }

  def genRectangles():Vector[Rectangle] =
  {
    val out = Vector[Rectangle](6)

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

  def genVertices():Vector[vec3] =
  {
    val out = Vector[vec3](8)

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

  def genVerticesCounterClockwise():Vector[vec3] =
  {
    val out = Vector[vec3](8)

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
    "OBB(center = " + center + ";right = " + right +  ";up = " + up + ";extent = " + vec3(extentRight,extentUp,extentLook) + ")"

  }
}

object OBB
{
  def apply(aabb: AABB): OBB = new OBB(aabb)

}



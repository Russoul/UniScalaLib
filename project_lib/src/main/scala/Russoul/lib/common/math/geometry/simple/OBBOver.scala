package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.TypeClasses.Field
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.math.algebra.{Mat, Mat4}
import Russoul.lib.common.{Real, Real3, V3, immutable}
import Russoul.lib.common.math.geometry.simple.general.CenteredShape3
import Russoul.lib.common.utils.Arr
import Russoul.lib.common.Implicits._

import scala.reflect.ClassTag

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class OBBOver[V : ClassTag, @specialized F : ClassTag : Field](center:V, right:V, up:V, extentRight:F, extentUp:F, extentLook:F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F] with CanonicalCrossProductOp[V])  extends CenteredShape3[V,F] {
  assert(ev.dimensions == 3)


  def this(aabb :AABBOver[V,F])(implicit ev: CanonicalEuclideanSpaceOverField[V,F] with CanonicalCrossProductOp[V])
  {
    this(aabb.center, ev.create(ev.scalar.one,ev.scalar.zero,ev.scalar.zero), ev.create(ev.scalar.zero,ev.scalar.one,ev.scalar.zero), aabb.extent.x, aabb.extent.y, aabb.extent.z)
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

  def genRightLine(length:F = ev.scalar.one): LineOver[V,F] =
  {
    LineOver(center, center + right * length)
  }

  def genUpLine(length:F = ev.scalar.one): LineOver[V,F] = {
    LineOver(center, center + up * length)
  }

  def genLookLine(length:F = ev.scalar.one): LineOver[V,F] = {
    LineOver[V,F](center, center + (right ⨯ up) * length)
  }


  def translate(tr:V): OBBOver[V,F] =
  {
    OBBOver(center + tr, right, up, extentRight, extentUp, extentLook)
  }

  def scale(s:F): OBBOver[V,F] =
  {
    OBBOver(center, right, up, extentRight * s, extentUp * s, extentLook * s)
  }




  def genRectangles():Arr[RectangleOver[V,F]] =
  {
    val out = Arr[RectangleOver[V,F]](6)

    val fe = (right ⨯ up) * extentLook
    val ue = up * extentUp
    val re = right * extentRight

    val t = center + ue
    val b = center - ue
    val l = center - re
    val r = center + re
    val ba = center - fe
    val f = center + fe

    out += new RectangleOver(t, re, -fe)
    out += new RectangleOver(b, re, fe)
    out += new RectangleOver(l, fe, ue)
    out += new RectangleOver(r, -fe, ue)
    out += new RectangleOver(ba, -re, ue)
    out += new RectangleOver(f, re, ue)

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
    "OBB(center = " + center + ";right = " + right +  ";up = " + up + ";extent = " + extentRight +";" + extentUp + ";" + extentLook + ")"

  }
}



class OBBOverReal(override val center:Real3, override val right:Real3, override val up:Real3, override val extentRight:Real, override val extentUp:Real, override val extentLook:Real)(implicit v3 : V3) extends OBBOver[Real3, Real](center, right, up, extentRight, extentUp, extentLook){

  def rotateAroundRight(rad:Real): OBBOver[Real3, Real] =
  {
    val mat = Mat4.matrixROTATIONRad(right, rad)
    new OBBOver(center,right,up ⨯ mat,extentRight, extentUp, extentLook)
  }


  def rotateAroundUp(rad:Real): OBBOver[Real3, Real] =
  {
    val mat = Mat4.matrixROTATIONRad(up, rad)
    new OBBOver(center,right ⨯ mat,up,extentRight, extentUp, extentLook)
  }


  def rotateAroundLook(rad:Real):OBBOver[Real3, Real] =
  {
    val mat = Mat4.matrixROTATIONRad(right ⨯ up, rad)
    new OBBOver(center,right ⨯ mat,up ⨯ mat,extentRight, extentUp, extentLook)
  }
}

object OBBOver
{
  def apply[V : ClassTag,@specialized F : ClassTag : Field](aabb: AABBOver[V,F])(implicit ev: CanonicalEuclideanSpaceOverField[V,F] with CanonicalCrossProductOp[V]): OBBOver[V,F] = new OBBOver[V,F](aabb)

}



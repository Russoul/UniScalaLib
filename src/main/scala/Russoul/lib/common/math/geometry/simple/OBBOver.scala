package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.TypeClasses.Field
import Russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import Russoul.lib.common.math.algebra.{Mat, Vec}
import Russoul.lib.common.{Real, Real3, V3, immutable}
import Russoul.lib.common.math.geometry.simple.general.CenteredShape3
import Russoul.lib.common.utils.Arr
import Russoul.lib.common.Implicits._
import shapeless.Nat._
import Russoul.lib.common._
import shapeless.Nat

import scala.reflect.ClassTag

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable class OBBOver[V[_,_ <: Nat], @specialized F : ClassTag : Field]private[geometry](val center:V[F,_3], val right:V[F,_3], val up:V[F,_3], val extentRight:F, val extentUp:F, val extentLook:F)(implicit evTag: ClassTag[V[F,_3]],ev: CanonicalEuclideanSpaceOverField[V,F,_3] , cross : CrossProductOverCanonicalEuclideanSpaceOverField[V,F], tensor1: Tensor1[F,V,_3])  extends CenteredShape3[V[F,_3],F] {


  def this(aabb :AABBOver[V,F])(implicit evTag: ClassTag[V[F,_3]], ev: CanonicalEuclideanSpaceOverField[V,F,_3] , cross : CrossProductOverCanonicalEuclideanSpaceOverField[V,F], tensor1: Tensor1[F,V,_3])
  {
    this(aabb.center, makeVector(_3,ev.scalar.one,ev.scalar.zero,ev.scalar.zero), makeVector(_3,ev.scalar.zero,ev.scalar.one,ev.scalar.zero), aabb.extent.x, aabb.extent.y, aabb.extent.z)
  }

  def genMax(): V[F,_3] =
  {
    val fe = (right ⨯ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    center + ue + re + fe
  }

  def genMin(): V[F,_3] =
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


  def translate(tr:V[F,_3]): OBBOver[V,F] =
  {
    new OBBOver(center + tr, right, up, extentRight, extentUp, extentLook)
  }

  def scale(s:F): OBBOver[V,F] =
  {
    new OBBOver(center, right, up, extentRight * s, extentUp * s, extentLook * s)
  }




  def genRectangles():Array[RectangleOver[V,F]]=
  {
    val out = new Array[RectangleOver[V,F]](6)

    val fe = (right ⨯ up) * extentLook
    val ue = up * extentUp
    val re = right * extentRight

    val t = center + ue
    val b = center - ue
    val l = center - re
    val r = center + re
    val ba = center - fe
    val f = center + fe

    out(0) = RectangleOver[V,F](t, re, -fe)
    out(1) = RectangleOver[V,F](b, re, fe)
    out(2) = RectangleOver[V,F](l, fe, ue)
    out(3) = RectangleOver[V,F](r, -fe, ue)
    out(4) = RectangleOver[V,F](ba, -re, ue)
    out(5) = RectangleOver[V,F](f, re, ue)

    out
  }

  def genVertices():Array[V[F,_3]] =
  {
    val out = new Array[V[F,_3]](8)

    val fe = (right ⨯ up)*extentLook
    val ue =  up*extentUp
    val re = right*extentRight

    out(0) = center + ue + re + fe
    out(1) = center + ue + re - fe
    out(2) = center + ue - re - fe
    out(3) = center + ue - re + fe

    out(4) = center - ue + re + fe
    out(5) = center - ue + re - fe
    out(6) = center - ue - re - fe
    out(7) = center - ue - re + fe

    out
  }

  def genVerticesCounterClockwise(): Array[V[F,_3]] =
  {
    val out = new Array[V[F,_3]](8)

    val l = (right ⨯ up)*extentLook
    val u =  up*extentUp
    val r = right*extentRight


    out(0) = center - u - l - r
    out(1) = center - u - l + r
    out(2) = center - u + l + r
    out(3) = center - u + l - r

    out(4) = center + u - l - r
    out(5) = center + u - l + r
    out(6) = center + u + l + r
    out(7) = center + u + l - r

    out
  }

  override def toString(): String =
  {
    "OBB(center = " + center + ";right = " + right +  ";up = " + up + ";extent = " + extentRight +";" + extentUp + ";" + extentLook + ")"

  }
}

object OBBOver{
  def apply[V[_,_ <: Nat], @specialized F : ClassTag : Field](center:V[F,_3], right:V[F,_3], up:V[F,_3], extentRight:F, extentUp:F, extentLook:F)(implicit evTag: ClassTag[V[F,_3]], ev: CanonicalEuclideanSpaceOverField[V,F,_3] , cross : CrossProductOverCanonicalEuclideanSpaceOverField[V,F], tensor1: Tensor1[F,V,_3]) = new OBBOver[V,F](center, right, up, extentRight, extentUp, extentLook)
  def apply[V[_,_ <: Nat], @specialized F : ClassTag : Field](aabb: AABBOver[V,F])(implicit evTag: ClassTag[V[F,_3]],  ev: CanonicalEuclideanSpaceOverField[V,F,_3] , cross : CrossProductOverCanonicalEuclideanSpaceOverField[V,F], tensor1: Tensor1[F,V,_3]): OBBOver[V,F] = new OBBOver[V,F](aabb)

}


class OBBOverReal private(override val center:Real3, override val right:Real3, override val up:Real3, override val extentRight:Real, override val extentUp:Real, override val extentLook:Real) extends OBBOver[Vec, Real](center, right, up, extentRight, extentUp, extentLook){

  def rotateAroundRight(rad:Real): OBBOver[Vec, Real] =
  {
    val mat = Mat4D.matrixROTATIONRad(right, rad)
    OBBOver(center,right,up = {val t = Real4(up,0D); val r = t * mat; Real3(r.x,r.y,r.z)},extentRight, extentUp, extentLook)
  }


  def rotateAroundUp(rad:Real): OBBOver[Vec, Real] =
  {
    val mat = Mat4D.matrixROTATIONRad(up, rad)

    OBBOver(center,right = {val t = Real4(right,0); val r = t * mat ; Real3(r.x,r.y,r.z)},up,extentRight, extentUp, extentLook)
  }


  def rotateAroundLook(rad:Real):OBBOver[Vec, Real] =
  {
    val mat = Mat4D.matrixROTATIONRad(right ⨯ up, rad)
    OBBOver(center,right = {val t = Real4(right,0); val r = t * mat ; Real3(r.x,r.y,r.z)},
      up = {val t = Real4(up,0D); val r = t * mat; Real3(r.x,r.y,r.z)},extentRight, extentUp, extentLook)
  }
}

object OBBOverReal{
  def apply(center: Real3, right: Real3, up : Real3, extentRight: Real, extentUp: Real, extentLook: Real)(implicit v3 : V3) = new OBBOverReal(center, right, up, extentRight, extentUp, extentLook)
}




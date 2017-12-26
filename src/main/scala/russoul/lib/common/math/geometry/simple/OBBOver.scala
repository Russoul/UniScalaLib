package russoul.lib.common.math.geometry.simple

import russoul.lib.common.math.algebra.{Mat, Row}
import russoul.lib.common.utils.Arr
import russoul.lib.common.math.geometry.simple.general.{CenteredShape, GeometricShape}

import scala.reflect.ClassTag

import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._
import singleton.ops._
/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class OBBOver[@tbsp F]private[geometry](override val center:Vec3[F], val right:Vec3[F], val up:Vec3[F], val extentRight:F, val extentUp:F, val extentLook:F) extends CenteredShape[F,_3] {


  private[geometry] def this(aabb :AABBOver[F])(implicit field: Field[F], tag : ClassTag[F]) {
    this(aabb.center, Vec3[F](field.one,field.zero,field.zero), Vec3[F](field.zero,field.one,field.zero), aabb.extent(0), aabb.extent(1), aabb.extent(2))
  }

  
  def genMax()(implicit field: Field[F], tag : ClassTag[F]): Vec3[F] =
  {
    val fe = (right ⨯ up) :* extentLook
    val ue =  up :* extentUp
    val re = right :* extentRight

    center + ue + re + fe
  }

  def genMin()(implicit field: Field[F], tag : ClassTag[F]): Vec3[F] =
  {
    val fe = (right ⨯ up) :* extentLook
    val ue =  up :* extentUp
    val re = right :* extentRight

    center - ue - re - fe
  }

  def genRightLine(length:F)(implicit field: Field[F], tag : ClassTag[F]): LineOver[F] =
  {
    LineOver(center, center + (right :* length))
  }

  def genUpLine(length:F)(implicit field: Field[F], tag : ClassTag[F]): LineOver[F] = {
    LineOver(center, center + (up :* length))
  }

  def genLookLine(length:F)(implicit field: Field[F], tag : ClassTag[F]): LineOver[F] = {
    LineOver[F](center, center + ((right ⨯ up) :* length))
  }


  override def translate(tr:Vec3[F])(implicit field: Field[F], tag : ClassTag[F]): OBBOver[F] =
  {
    new OBBOver(center + tr, right, up, extentRight, extentUp, extentLook)
  }

  def scale(s:F)(implicit field: Field[F], tag : ClassTag[F]): OBBOver[F] =
  {
    new OBBOver(center, right, up, extentRight * s, extentUp * s, extentLook * s)
  }


  override def scaleAroundBasis(factor: F)(implicit field: Field[F], tag : ClassTag[F]): OBBOver[F] = {
    new OBBOver(center :* factor, right, up, extentRight * factor, extentUp * factor, extentLook * factor)
  }



  def genRectangles()(implicit field: Field[F], tag: ClassTag[Vec3[F]], tag2 : ClassTag[F]): Array[RectangleOver[F]]=
  {
    val out = new Array[RectangleOver[F]](6)

    val fe = (right ⨯ up) :* extentLook
    val ue = up :* extentUp
    val re = right :* extentRight

    val t = center + ue
    val b = center - ue
    val l = center - re
    val r = center + re
    val ba = center - fe
    val f = center + fe

    out(0) = RectangleOver[F](t, re, -fe)
    out(1) = RectangleOver[F](b, re, fe)
    out(2) = RectangleOver[F](l, fe, ue)
    out(3) = RectangleOver[F](r, -fe, ue)
    out(4) = RectangleOver[F](ba, -re, ue)
    out(5) = RectangleOver[F](f, re, ue)

    out
  }

  def genVertices()(implicit field: Field[F], tag: ClassTag[Vec3[F]], tag2 : ClassTag[F]):Array[Vec3[F]] =
  {
    val out = new Array[Vec3[F]](8)

    val fe = (right ⨯ up) :* extentLook
    val ue =  up :* extentUp
    val re = right :* extentRight

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

  def genVerticesCounterClockwise()(implicit field: Field[F], tag: ClassTag[Vec3[F]], tag2 : ClassTag[F]): Array[Vec3[F]] =
  {
    val out = new Array[Vec3[F]](8)

    val l = (right ⨯ up) :* extentLook
    val u =  up :* extentUp
    val r = right :* extentRight


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
  def apply[@tbsp F](center:Vec3[F], right:Vec3[F], up:Vec3[F], extentRight:F, extentUp:F, extentLook:F) = new OBBOver[F](center, right, up, extentRight, extentUp, extentLook)
  def apply[@tbsp F](aabb: AABBOver[F])(implicit field: Field[F], tag : ClassTag[F]): OBBOver[F] = new OBBOver[F](aabb)

}


class OBBOverDouble private(override val center:Double3, override val right:Double3, override val up:Double3, override val extentRight:Double, override val extentUp:Double, override val extentLook:Double) extends OBBOver[Double](center, right, up, extentRight, extentUp, extentLook){

  
  
  def rotateAroundRight(rad:Double): OBBOver[Double] =
  {
    val mat = Mat4D.rotationRad(right, rad)
    OBBOver(center,right,up = {val t = Double4(up,1D); val r = t ⨯ mat; Double3(r(0),r(1),r(2))},extentRight, extentUp, extentLook)
  }


  def rotateAroundUp(rad:Double): OBBOver[Double] =
  {
    val mat = Mat4D.rotationRad(up, rad)

    OBBOver(center,right = {val t = Double4(right,1D); val r = t ⨯ mat ; Double3(r(0),r(1),r(2))},up,extentRight, extentUp, extentLook)
  }


  def rotateAroundLook(rad:Double):OBBOver[Double] =
  {
    val mat = Mat4D.rotationRad(right ⨯ up, rad)
    OBBOver(center,right = {val t = Double4(right,1D); val r = t ⨯ mat ; Double3(r(0),r(1),r(2))},
      up = {val t = Double4(up,1D); val r = t ⨯ mat; Double3(r(0),r(1),r(2))},extentRight, extentUp, extentLook)
  }
}

object OBBOverDouble{
  def apply(center: Double3, right: Double3, up : Double3, extentRight: Double, extentUp: Double, extentLook: Double) = new OBBOverDouble(center, right, up, extentRight, extentUp, extentLook)
}




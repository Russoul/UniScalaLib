package Russoul.lib.common.math.immutable.geometry.simple

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._
import Russoul.lib.common.math.immutable.geometry.simple.general.Shape3
import Russoul.lib.common.math.immutable.linear.{mat4, Vec2, Vec3}
import Russoul.lib.common.utils.Vector


/**
  * @note extents must be orthogonal
  *
  *       ^^
  *  up   ||
  *       ||
  *       ||
  * right XX----->
  */
@immutable case class Rectangle[A : FieldLike](center: Vec3[A], right: Vec3[A], up: Vec3[A]) extends Shape3[A] {


  override def translate(v: Vec3[A]): Rectangle[A] = {
    Rectangle(center + v, right, up)
  }

  /**
    *
    * @return right hand rule
    */
  def genNormal(): Vec3[A] = right.crossProduct(up).normalize()



  def genVerticesClockwise(): Vector[Vec3[A]] = Vector[Vec3[A]](center + up - right, center + up + right, center - up + right, center - up - right)

  def genVertices(): Vector[Vec3[A]] = Vector[Vec3[A]](center - up - right, center - up + right, center + up + right, center + up - right)

  def scale(right:A, up:A): Rectangle[A] =
  {
    new Rectangle(center, this.right * right, this.up * up)
  }

  def scaleAroundBasis(scale:A): Rectangle[A] =
  {
    new Rectangle(center*scale, this.right * scale, this.up * scale)
  }

  def scaleAroundBasisZConst(scale:A): Rectangle[A] =
  {
    new Rectangle(Vec3(center.x*scale, center.y * scale,center.z), this.right * scale, this.up * scale)
  }


  override def toString: String =
  {
    "Rectangle( center = " + center.toString() + "; right = " + right.toString() + "; up = " + up.toString() + " )"
  }
}

object Rectangle
{
  def fromMinMax2DParallelToZ[A](min:Vec2[A], max:Vec2[A], z:A)(implicit ev: FieldLike[A]): Rectangle[A] =
  {
    val t = Vec3(max,ev.zero) - Vec3(min,ev.zero)
    new Rectangle(Vec3(min,z) + t*ev.fromDouble(0.5D), Vec3(t.x/ev.fromDouble(2D),ev.zero,ev.zero), Vec3(ev.zero,t.y/ev.fromDouble(2D),ev.zero))
  }
}

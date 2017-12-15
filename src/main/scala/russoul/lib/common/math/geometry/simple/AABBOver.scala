package russoul.lib.common.math.geometry.simple

import russoul.lib.common.TypeClasses._
import russoul.lib.common.utils.Arr
import russoul.lib.common.immutable
import russoul.lib.common.Implicits._
import shapeless.Nat._
import russoul.lib.common._
import russoul.lib.common.math.geometry.simple.general.{CenteredShape, GeometricShape}
import shapeless.Nat
import Abstraction._
import russoul.lib.common.math.algebra.Vec

import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.implicits._

@immutable case class AABBOver[@tbsp F]private (override val center: Vec[F,_3],val extent: Vec[F,_3]) extends CenteredShape[F,_3] {

  def genMin()(implicit field: Field[F]): Vec[F,_3] = center - extent
  def genMax()(implicit field: Field[F]): Vec[F,_3] = center + extent

  override def translate(v: Vec[F,_3])(implicit field: Field[F]): AABBOver[F] =
  {
    new AABBOver(center + v, extent)
  }

  /**
    *
    * @return scaled version (around AABB's center point)
    */
  override def scale(s:F)(implicit field: Field[F]): AABBOver[F] =
  {
    new AABBOver(center, extent * s)
  }


  override def scaleAroundBasis(factor: F)(implicit ev3: Field[F]): AABBOver[F] = {
    new AABBOver(center * factor, extent * factor)
  }

  def genVertices()(implicit tag: ClassTag[Vec[F,_3]], f : Field[F]): Array[Vec[F,_3]] =
  {
    val a = new Array[Vec[F,_3]](8)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a(0) = Vec[F, _3](center.x-sx, center.y-sy, center.z-sz)
    a(1) = Vec[F, _3](center.x-sx, center.y-sy, center.z+sz)
    a(2) = Vec[F, _3](center.x+sx, center.y-sy, center.z+sz)
    a(3) = Vec[F, _3](center.x+sx, center.y-sy, center.z-sz)
    a(4) = Vec[F, _3](center.x-sx, center.y+sy, center.z-sz)
    a(5) = Vec[F, _3](center.x-sx, center.y+sy, center.z+sz)
    a(6) = Vec[F, _3](center.x+sx, center.y+sy, center.z+sz)
    a(7) = Vec[F, _3](center.x+sx, center.y+sy, center.z-sz)

    a
  }

  /**
    *
    *
    */
  def genRectangles()(implicit field: Field[F], tag: ClassTag[V[F,_3]]): Array[RectangleOver[F]] =
  {

    val a = new Array[RectangleOver[F]](6)

    val sx = extent.x
    val sy = extent.y
    val sz = extent.z


    a(0) = RectangleOver[V,F](center + makeVector(_3,field.zero, sy, field.zero), makeVector(_3,sx, field.zero,field.zero), makeVector(_3,field.zero,field.zero,-sz))//top
    a(1) = RectangleOver[V,F](center + makeVector(_3,field.zero, -sy, field.zero), makeVector(_3,sx, field.zero,field.zero), makeVector(_3,field.zero,field.zero,sz))//bottom
    a(2) = RectangleOver[V,F](center + makeVector(_3,-sx, field.zero, field.zero), makeVector(_3,field.zero, field.zero,sz), makeVector(_3,field.zero,sy,field.zero))//left
    a(3) = RectangleOver[V,F](center + makeVector(_3,sx, field.zero, field.zero), makeVector(_3,field.zero, field.zero,-sz), makeVector(_3,field.zero,sy,field.zero))//right
    a(4) = RectangleOver[V,F](center + makeVector(_3,field.zero, field.zero, -sz), makeVector(_3,-sx, field.zero,field.zero), makeVector(_3,field.zero,sy,field.zero))//back
    a(5) = RectangleOver[V,F](center + makeVector(_3,field.zero, field.zero, sz), makeVector(_3,sx, field.zero,field.zero), makeVector(_3,field.zero,sy,field.zero))//front
    
    
    a
  }

  override def toString(): String =
  {
    "AABB(center = " + center + ";extent = " + extent + ")"

  }

}

object AABBOver
{
  def genFromMinMax[V[_,_ <: Nat],@tbsp F](min:V[F,_3], max:V[F,_3])(implicit field: Field[F], tag: ClassTag[V[F,_3]], con: Con[F]):AABBOver[V,F] =
  {
    val extent = (max-min) * 0.5D.as[F]
    val center = min + extent

    new AABBOver[V,F](center,extent)
  }

  def apply[V[_,_ <: Nat], @tbsp F](center: V[F,_3], extent: V[F,_3]) = new AABBOver[V,F](center, extent)
}

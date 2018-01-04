package russoul.lib.common.math.geometry.simple

import russoul.lib.common.utils.Arr
import russoul.lib.common.math.geometry.simple.general.{CenteredShape, GeometricShape}
import russoul.lib.common.math.algebra.Row

import scala.reflect.ClassTag
import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._


@immutable case class AABBOver[@specialized(Float,Double,Int) F] (override val center: Row[F,_3], val extent: Row[F,_3]) extends CenteredShape[F,_3] {

  def genMin()(implicit field: Field[F], tag : ClassTag[F]): Row[F,_3] = center - extent
  def genMax()(implicit field: Field[F], tag : ClassTag[F]): Row[F,_3] = center + extent

  override def translate(v: Row[F,_3])(implicit field: Field[F], tag : ClassTag[F]): AABBOver[F] =
  {
    new AABBOver(center + v, extent)
  }

  /**
    *
    * @return scaled version (around AABB's center point)
    */
  override def scale(s:F)(implicit field: Field[F], tag : ClassTag[F]): AABBOver[F] =
  {
    new AABBOver(center, extent :* s)
  }


  override def scaleAroundBasis(factor: F)(implicit ev3: Field[F], tag : ClassTag[F]): AABBOver[F] = {
    new AABBOver(center :* factor, extent :* factor)
  }

  def genVertices()(implicit f : Field[F], tag : ClassTag[F]): Array[Row[F,_3]] =
  {
    val a = new Array[Row[F,_3]](8)

    val sx = extent(0)
    val sy = extent(1)
    val sz = extent(2)


    a(0) = Vec3[F](center(0)-sx, center(1)-sy, center(2)-sz)
    a(1) = Vec3[F](center(0)-sx, center(1)-sy, center(2)+sz)
    a(2) = Vec3[F](center(0)+sx, center(1)-sy, center(2)+sz)
    a(3) = Vec3[F](center(0)+sx, center(1)-sy, center(2)-sz)
    a(4) = Vec3[F](center(0)-sx, center(1)+sy, center(2)-sz)
    a(5) = Vec3[F](center(0)-sx, center(1)+sy, center(2)+sz)
    a(6) = Vec3[F](center(0)+sx, center(1)+sy, center(2)+sz)
    a(7) = Vec3[F](center(0)+sx, center(1)+sy, center(2)-sz)

    a
  }

  /**
    *
    *
    */
  def genRectangles()(implicit field: Field[F], tag: ClassTag[F]): Array[RectangleOver[F]] =
  {

    val a = new Array[RectangleOver[F]](6)

    val sx = extent(0)
    val sy = extent(1)
    val sz = extent(2)


    a(0) = RectangleOver[F](center + Vec3[F](field.zero, sy, field.zero), Vec3[F](sx, field.zero,field.zero), Vec3[F](field.zero,field.zero,-sz))//top
    a(1) = RectangleOver[F](center + Vec3[F](field.zero, -sy, field.zero), Vec3[F](sx, field.zero,field.zero), Vec3[F](field.zero,field.zero,sz))//bottom
    a(2) = RectangleOver[F](center + Vec3[F](-sx, field.zero, field.zero), Vec3[F](field.zero, field.zero,sz), Vec3[F](field.zero,sy,field.zero))//left
    a(3) = RectangleOver[F](center + Vec3[F](sx, field.zero, field.zero), Vec3[F](field.zero, field.zero,-sz), Vec3[F](field.zero,sy,field.zero))//right
    a(4) = RectangleOver[F](center + Vec3[F](field.zero, field.zero, -sz), Vec3[F](-sx, field.zero,field.zero), Vec3[F](field.zero,sy,field.zero))//back
    a(5) = RectangleOver[F](center + Vec3[F](field.zero, field.zero, sz), Vec3[F](sx, field.zero,field.zero), Vec3[F](field.zero,sy,field.zero))//front
    
    
    a
  }

  override def toString(): String =
  {
    "AABB(center = " + center + ";extent = " + extent + ")"

  }

}

object AABBOver
{
  def genFromMinMax[@specialized(Float,Double,Int) F](min:Vec3[F], max:Vec3[F])(implicit field: Field[F], tag: ClassTag[F]):AABBOver[F] =
  {
    val extent = (max-min) * field.fromDouble(0.5D)
    val center = min + extent

    new AABBOver[F](center,extent)
  }

  //def apply[@specialized(Float,Double,Int) F : ClassTag](center: Vec3[F], extent: Vec3[F]) = new AABBOver[F](center, extent)
}

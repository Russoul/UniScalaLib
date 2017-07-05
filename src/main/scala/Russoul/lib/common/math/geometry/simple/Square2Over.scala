package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.utils.Arr

import scala.reflect.ClassTag
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Square2Over [V : ClassTag,@specialized F : Field](center:V, extent:F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F])  extends CenteredShape2[V,F]{
  assert(ev.dimensions == 2)



  override def translate(v: V): Square2Over[V,F] = {
    Square2Over(center + v, extent)
  }

  override def scaleAroundBasis(factor: F): Square2Over[V,F] = {
    Square2Over(center * factor, extent * factor)
  }

  def genVertices(): Arr[V] = Arr(center - ev.create(extent,extent), center + ev.create(extent, -extent), center + ev.create(extent,extent), center + ev.create(-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F): Square2Over[V,F] =
  {
    Square2Over(center, extent * scalar)
  }

  def toRectangle2():Rectangle2Over[V,F] =
  {
    Rectangle2Over[V,F](center, ev.create(extent, extent))
  }

  override def toString: String =
  {
    "Square2(center = "+center.toString() + ";extent = " + extent + ")"
  }

}

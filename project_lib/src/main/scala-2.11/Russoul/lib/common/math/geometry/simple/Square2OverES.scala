package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.TypeClasses.{EuclideanSpaceOverField, Field}
import Russoul.lib.common.TypeClasses.EuclideanSpaceOverField.Implicits._
import Russoul.lib.common.TypeClasses.Field.Implicits._
import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.math.linear.{Vec2, Vec3}
import Russoul.lib.common.utils.Arr

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
@immutable case class Square2OverES [V,@specialized F](center:V, extent:F)(implicit ev: EuclideanSpaceOverField[V,F])  extends CenteredShape2[V,F]{




  override def translate(v: V): Square2OverES[V,F] = {
    Square2OverES(center + v, extent)
  }

  override def scaleAroundBasis(factor: F): Square2OverES[V,F] = {
    Square2OverES(center * factor, extent * factor)
  }

  def genVertices(): Arr[V] = Arr(center - ev.create(extent,extent), center + ev.create(extent, -extent), center + ev.create(extent,extent), center + ev.create(-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F): Square2OverES[V,F] =
  {
    Square2OverES(center, extent * scalar)
  }

  def toRectangle2():Rectangle2OverES[V,F] =
  {
    Rectangle2OverES(center, ev.create(extent, extent))
  }

  override def toString: String =
  {
    "Square2(center = "+center.toString() + ";extent = " + extent + ")"
  }

}

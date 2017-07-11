package Russoul.lib.common.math.geometry.simple

import Russoul.lib.common.immutable
import Russoul.lib.common.math.geometry.simple.general.CenteredShape2
import Russoul.lib.common.utils.Arr

import scala.reflect.ClassTag
import Russoul.lib.common.Implicits._
import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, Field}
import shapeless.Nat._

/**
  * Created by russoul on 22.04.17.
  *
  *
  * AXIS ALIGNED !!!
  */
@immutable class Square2Over[V[_,_] : ClassTag,@specialized F : Field](val center:V[F,_2], val extent:F)(implicit ev: CanonicalEuclideanSpaceOverField[V,F,_2])  extends CenteredShape2[V[F,_2],F]{



  override def translate(v: V[F,_2]): Square2Over[V,F] = {
    new Square2Over(center + v, extent)
  }

  override def scaleAroundBasis(factor: F): Square2Over[V,F] = {
    new Square2Over(center * factor, extent * factor)
  }

  def genVertices(): Array[V[F,_2]] = Array(center - ev.staticContainer.factory.makeVector(extent,extent), center + ev.staticContainer.factory.makeVector[_2](extent, -extent), center + ev.staticContainer.factory.makeVector[_2](extent,extent), center + ev.staticContainer.factory.makeVector[_2](-extent, extent))


  /**
    * scaling around center of this rectangle
    */
  def scale(scalar:F): Square2Over[V,F] =
  {
    new Square2Over(center, extent * scalar)
  }

  def toRectangle2():Rectangle2Over[V,F] =
  {
    Rectangle2Over[V,F](center, ev.staticContainer.factory.makeVector[_2](extent, extent))
  }

  override def toString: String =
  {
    "Square2(center = "+center.toString() + ";extent = " + extent + ")"
  }

}

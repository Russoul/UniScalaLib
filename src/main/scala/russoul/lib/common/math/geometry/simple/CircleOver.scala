package russoul.lib.common.math.geometry.simple

import russoul.lib.common.math.algebra.Vec
import russoul.lib.common.math.geometry.simple.general.CenteredShape

import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 23.04.17.
  */
@immutable case class CircleOver[@tbsp F]private(override val center:Vec[F,_2],val rad:F) extends CenteredShape[F,_2] {


  override def translate(v: Vec[F,_2])(implicit field: Field[F]): CircleOver[F] = {
    new CircleOver(center + v, rad)
  }

  override def scale(scalar:F)(implicit field: Field[F]): CircleOver[F] = {
    new CircleOver(center, rad * scalar)
  }

  override def scaleAroundBasis(scalar:F)(implicit field: Field[F]): CircleOver[F] = {
    new CircleOver(center * scalar, rad * scalar)
  }

  def inscribedInRectangle2()(implicit field: Field[F]): Rectangle2Over[F] = {
    Rectangle2Over[F](center, Vec[F,_2](rad,rad))
  }

  override def toString(): String =
  {
    "Circle( center = " + center + "; radius = " + rad + " )"
  }
}

object CircleOver{
  def apply[@tbsp F](center:Vec2[F], rad:F) = new CircleOver[F](center, rad)
}

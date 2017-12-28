package russoul.lib.common.math.geometry.simple

import russoul.lib.common.math.geometry.simple.general.{CenteredShape, GeometricShape}
import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

import scala.reflect.ClassTag

/**
  * Created by Russoul on 21.04.2016.
  */
@immutable case class SphereOver[@specialized(Float,Double,Int) F] (override val center:Vec3[F],val rad: F) extends CenteredShape[F,_3] {

  override def translate(v: Vec3[F])(implicit ev3: Field[F], tag : ClassTag[F]): SphereOver[F] = {
    new SphereOver(center + v, rad)
  }


  override def scaleAroundBasis(factor: F)(implicit ev3: Field[F], tag : ClassTag[F]): SphereOver[F] = {
    new SphereOver(center :* factor, rad * factor)
  }

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  override def scale(factor: F)(implicit ev3: Field[F], tag : ClassTag[F]): SphereOver[F] = {
    new SphereOver(center, factor * rad)
  }

  override def toString: String = {
    "Sphere(center = " + center.toString() + ";radius = " + rad + " )"
  }
}

object SphereOver{
  def apply[@specialized(Float,Double,Int) F](center:Vec3[F],rad: F) = new SphereOver[F](center, rad)
}

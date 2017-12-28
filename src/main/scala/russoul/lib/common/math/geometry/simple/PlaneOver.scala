package russoul.lib.common.math.geometry.simple

import russoul.lib.common.math.geometry.simple.general.GeometricShape
import russoul.lib.common._
import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

import scala.reflect.ClassTag

/**
  * Created by Russoul on 18.07.2016.
  */
@immutable case class PlaneOver[@specialized(Float,Double,Int) F](val point:Vec3[F],val normal:Vec3[F]) extends GeometricShape[F,_3]{


  override def translate(v: Vec3[F])(implicit ev3: Field[F], tag : ClassTag[F]): PlaneOver[F] = new PlaneOver[F](point, normal)


  override def scaleAroundBasis(factor: F)(implicit ev3: Field[F], tag : ClassTag[F]): PlaneOver[F] = {
    new PlaneOver(point :* factor, normal)
  }

  override def toString(): String = {
    "Plane( point = " + point + "; normal = " + normal + " )"
  }

}

object PlaneOver{
  def apply[@specialized(Float,Double,Int) F](point:Vec3[F], normal:Vec3[F]) = new PlaneOver[F](point, normal)
}
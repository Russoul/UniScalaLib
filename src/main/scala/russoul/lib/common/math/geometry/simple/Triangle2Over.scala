package russoul.lib.common.math.geometry.simple

import russoul.lib.common.Abstraction.{CES, T1}
import russoul.lib.common.math.geometry.simple.general.GeometricShape
import russoul.lib.common.{TypeClasses, immutable, tbsp}
import shapeless.Nat
import shapeless.Nat._
import russoul.lib.common._
import Implicits._

/**
  * Created by russoul on 17.07.2017.
  */
@immutable case class Triangle2Over[V[_,_ <: Nat], @tbsp F] private(val p1:V[F,_2], val p2:V[F,_2], val p3:V[F,_2]) extends GeometricShape[V,F,_2] {

  

  override def translate(v: V[F, _2])(implicit ev1: CES[V, F, _2], ev2: T1[F, V, _2], ev3: TypeClasses.Field[F]): Triangle2Over[V, F] = {
    new Triangle2Over(p1 + v, p2 + v, p3 + v)
  }

  override def scaleAroundBasis(factor: F)(implicit ev1: CES[V, F, _2], ev2: T1[F, V, _2], ev3: TypeClasses.Field[F]): Triangle2Over[V, F] = {
    new Triangle2Over(p1 * factor, p2 * factor, p3 * factor)
  }

  override def toString: String = {
    "Triangle2(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
  }
}

object Triangle2Over{
  @inline def apply[V[_,_ <: Nat], @tbsp F](p1:V[F,_2], p2:V[F,_2], p3:V[F,_2]) = new Triangle2Over[V,F](p1,p2,p3)
}

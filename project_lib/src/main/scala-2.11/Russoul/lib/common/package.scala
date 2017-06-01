package Russoul.lib


import Russoul.lib.common.math.immutable.algebra.ComplexOverField
import Russoul.lib.common.math.immutable.linear.{Vec2, Vec3, Vec4}

import scala.language.implicitConversions


/**
  * Created by russoul on 11.05.17.
  */
package object common
{

  type Real = Double
  type Complex = ComplexOverField[Real]
  type Real2 = Vec2[Real]
  type Real3 = Vec3[Real]
  type Real4 = Vec4[Real]

  type Int2 = (Int,Int)
  type Int3 = (Int,Int,Int)

  def nil[T <: Any]: T =
  {
    null.asInstanceOf[T]
  }





}

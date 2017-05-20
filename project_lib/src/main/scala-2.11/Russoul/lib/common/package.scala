package Russoul.lib

import Russoul.lib.common.math.immutable.algebra.Real

import scala.language.implicitConversions


/**
  * Created by russoul on 11.05.17.
  */
package object common
{


  def nil[T <: Any]: T =
  {
    null.asInstanceOf[T]
  }




}

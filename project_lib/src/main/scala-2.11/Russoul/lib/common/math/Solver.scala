package Russoul.lib.common.math

import Russoul.lib.common.math.immutable.linear.vec2

/**
  * Created by russoul on 13.05.17.
  */
object Solver
{

  /**
    * a1X + a0 = 0
    */
  @inline def findRealRootsPolynomial1(a1:Float, a0:Float): Option[Float] ={
    if(a1 != 0) Some(-a0/a1) else None
  }


  @inline def findDiscriminantOfPolynomial2(a2:Float, a1:Float, a0:Float) : Float =
  {
    a1 * a1 - 4 * a2 * a0
  }

  /**
    * a2X² + a1X + a0 = 0
    */
  /*@inline def findRealRootsPolynomial2(a2:Float, a1:Float, a0:Float) : Option[vec2] =
  {
    val disc = findDiscriminantOfPolynomial2(a2,a1,a0)

    if(D)
  }*/
}

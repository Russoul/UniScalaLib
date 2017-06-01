package Russoul.lib.common.math

import Russoul.lib.common.Real
import Russoul.lib.common.math.TypeClasses.{ConvertibleFromDouble, Field}
import Russoul.lib.common.math.TypeClasses.Field.Implicits._
import Russoul.lib.common.math.immutable.algebra.{Unknown, Vec}
import Russoul.lib.common.math.immutable.linear.Vec2

/**
  * Created by russoul on 13.05.17.
  */
object Solver
{

  /**
    * a1X + a0 = 0
    */
  @inline def findRealRootsPolynomial1[A](a1:A, a0:A)(implicit ev: Field[A]): Option[A] ={
    if(a1 != 0) Some(-a0/a1) else None
  }


  @inline def findDiscriminantOfPolynomial2[A](a2:A, a1:A, a0:A)(implicit ev: Field[A], con:ConvertibleFromDouble[A]) : A =
  {
    a1 * a1 - 4D.to * a2 * a0
  }

  /**
    * a2X² + a1X + a0 = 0
    */
  /*@inline def findRealRootsPolynomial2[A](a2:A, a1:A, a0:A)(implicit ev: Field[A], con:ConvertibleFromDouble[A]) : Option[Vec[Unknown,A]] =
  {
    val disc = findDiscriminantOfPolynomial2(a2,a1,a0)

    if(disc > 0){
      //val x1 =
    }
  }*/
}

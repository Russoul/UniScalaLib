package Russoul.lib


import Russoul.lib.common.TypeClasses.Field
import Russoul.lib.common.math.algebra.{ComplexOverField, Vec}
import Russoul.lib.common.math.geometry.simple.Ray2OverES
import Russoul.lib.common.math.linear.{Vec2, Vec3, Vec4}
import Russoul.lib.common.utils.Arr

import scala.annotation.Annotation
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
  type RealN = Vec[Real]


  object Real2{
    @inline def apply(x: Real, y: Real) = Vec2(x,y)
  }
  
  object Real3{
    @inline def apply(x: Real, y: Real, z: Real) = Vec3(x,y,z)
    @inline def apply(v2:Vec2[Real], z:Real) = Vec3(v2.x, v2.y, z)
  }
  
  object Real4{
    @inline def apply(x: Real, y: Real, z: Real, w: Real)  = Vec4(x,y,z,w)
    @inline def apply(v:Vec3[Real], w:Real): Vec4[Real] = Vec4(v.x, v.y, v.z, w)
  }
  
  object RealN{
    @inline def apply(seq: Real*):Vec[Real]  = {
      val ar = new Arr[Real](seq.size)
      for(i <- seq) ar += i

      Vec(ar)
    }
  }

  //same as RealN
  object Real{
    @inline def apply(seq: Real*):Vec[Real]  = {
      val ar = new Arr[Real](seq.size)
      for(i <- seq) ar += i

      Vec(ar)
    }
  }

  type Int2 = (Int,Int)
  type Int3 = (Int,Int,Int)

  @inline def nil[T <: Any]: T = null.asInstanceOf[T]


  /**
    * Created by russoul on 01.06.2017.
    */

  /**
    * this annotation is used on objects(classes, methods, functions, code blocks, etc)
    * if they contain code that does not perform checks on given input
    * primary use case: getting extra performance
    *
    * examples:
    *
    * "@straight" def sum2(a:Array[Int], b:Array[Int]) ={
    *   Array(a(0) + b(0), a(1) + b(1))
    * }
    * as you see this function does not perform common sense checks like bounds check or check for
    * correct size of input arrays (a.size == b.size && a.size == 2)
    *
    * so if object(class, method, ...) is marked "straight" then some sets of input values may lead to undefined behaviour
    * which may represent exception at best or incorrect computations at worst (most likely case)
    *
    */
  class straight extends Annotation

  /**
    * Created by russoul on 11.05.17.
    *
    * represents mutable data
    */
  class mutable extends Annotation

  /**
    * Created by russoul on 11.05.17.
    *
    *
    * represents immutable data
    */
  class immutable extends Annotation

  //used with context bounds to force the user to provide a type ("Nothing" is no longer valid)
  sealed trait NotNothing[-T]

  object NotNothing {
    implicit object YoureSupposedToSupplyAType extends NotNothing[Nothing]
    implicit object notNothing extends NotNothing[Any]
  }




}

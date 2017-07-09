package Russoul.lib


import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.math.algebra._
import Russoul.lib.common.math.geometry.simple._
import Russoul.lib.common.Implicits._
import shapeless.Nat

import scala.annotation.Annotation
import scala.language.implicitConversions


/**
  * Created by russoul on 11.05.17.
  */
package object common
{


  //Functional

  //TODO this currently does not work with functions that take implicit parameters, fixed in dotty ?
  implicit class Pipe1[A,R](a: A){
    def |>(f: A => R): R = f(a)
  }

  implicit class Pipe2[A,B,R](a: (A,B)){
    def ||>(f: (A,B) => R): R = f(a._1,a._2)
  }

  //............................................


  //specialization
  type sp = specialized
  //....

  //simple types---------------------------
  //Real is just another name for double, it must not be changed to float or any other similar types(there is actually no other similar types on JVM)
  type Real = Double //TODO changing this leads to errors, so dont !
  type Complex = ComplexOver[Real]
  type Real2 = Vec2[Real]
  type Real3 = Vec3[Real]
  type Real4 = Vec4[Real]
  type RealN = Vec[Real]

  type RealF = Float
  type RealD = Double
  type Real2F = Float2
  type Real3F = Float3
  type Real4F = Float4
  type Real2D = Real2
  type Real3D = Real3
  type Real4D = Real4

  type ComplexF = ComplexOver[Float]


  
  //The same as Real
  type Double2 = Vec2[Double]
  type Double3 = Vec3[Double]
  type Double4 = Vec4[Double]
  type DoubleN = Vec[Double]
  //...................
  
  type Float2 = Vec2[Float]
  type Float3 = Vec3[Float]
  type Float4 = Vec4[Float]
  type FloatN = Vec[Float]
  
  type Int2 = Vec2[Int]
  type Int3 = Vec3[Int]
  type Int4 = Vec4[Int]

  type Mat4D = Mat4[Double]
  type Mat4F = Mat4[Float]

  
  //--------------------------------------

  //Common simple geometric objects over reals--------------
  type AABB = AABBOver[Real3, Real]
  object AABB{
    def apply(center: Real3, extent: Real3) = AABBOver[Real3,Real](center, extent)
  }
  type Circle = CircleOver[Real2, Real]
  object Circle{
    def apply(center : Real2, rad: Real) = CircleOver(center, rad)
  }
  type Line2 = Line2Over[Real2, Real]
  object Line2{
    def apply(start : Real2, end : Real2): Line2 = Line2Over[Real2, Real](start, end)
  }
  type Line = LineOver[Real3, Real]
  object Line{
    def apply(start: Real3, end: Real3): Line = LineOver[Real3, Real](start, end)
  }
  type OBB = OBBOverReal
  object OBB{
    def apply(center: Real3, right: Real3, up: Real3, extentRight: Real, extentUp: Real, extentLook: Real): OBB = new OBBOverReal(center, right, up, extentRight, extentUp, extentLook)
  }
  type Plane = PlaneOver[Real3, Real]
  object Plane{
    def apply(point: Real3, normal: Real3): Plane = PlaneOver[Real3, Real](point, normal)
  }
  type Ray2 = Ray2Over[Real2, Real]
  object Ray2{
    def apply(start: Real2, end: Real2): Ray2 = Ray2Over[Real2,Real](start, end)
  }
  type Ray = RayOver[Real3, Real]
  object Ray{
    def apply(start: Real3, end: Real3): Ray = RayOver[Real3, Real](start, end)
  }
  type Rectangle2 = Rectangle2Over[Real2, Real]
  object Rectangle2{
    def apply(center: Real2, extent: Real2): Rectangle2 = Rectangle2Over[Real2,Real](center, extent)
  }
  type Rectangle = RectangleOver[Real3, Real]
  object Rectangle{
    def apply(center: Real3, right : Real3, up : Real3): Rectangle = RectangleOver[Real3, Real](center, right, up)
  }
  type Sphere = SphereOver[Real3, Real]
  object Sphere{
    def apply(center: Real3, rad: Real): Sphere = SphereOver[Real3, Real](center, rad)
  }
  type Square2 = Square2Over[Real2, Real]
  object Square2{
    def apply(center: Real2, extent: Real): Square2 = Square2Over[Real2, Real](center, extent)
  }
  type Triangle = TriangleOver[Real3, Real]
  object Triangle{
    def apply(p1: Real3, p2: Real3, p3: Real3): Triangle = TriangleOver[Real3, Real](p1, p2, p3)
  }
  type OBB2 = OBB2Over[Real2, Real]
  object OBB2{
    def apply(center: Real2, right: Real2, up: Real2, extentRight: Real, extentUp: Real): OBB2 = OBB2Over(center, right, up, extentRight, extentUp)
  }


  type AABBD = AABB
  type CircleD = Circle
  type Line2D = Line2
  type LineD = Line
  type OBBD = OBB
  type PlaneD = Plane
  type Ray2D = Ray2
  type RayD = Ray
  type Rectangle2D = Rectangle2
  type RectangleD = Rectangle
  type SphereD = Sphere
  type Square2D = Square2
  type TriangleD = Triangle
  type OBB2D = OBB2
  //--------------------------------------------------------




  //Common simple geometric objects over Floats--------------
  type AABBF = AABBOver[Float3, Float]
  object AABBF{
    def apply(center: Float3, extent: Float3) = AABBOver[Float3, Float](center, extent)
  }
  type CircleF = CircleOver[Float2, Float]
  object CircleF{
    def apply(center : Float2, rad: Float) = CircleOver(center, rad)
  }
  type Line2F = Line2Over[Float2, Float]
  object Line2F{
    def apply(start : Float2, end : Float2) = Line2Over[Float2, Float](start, end)
  }
  type LineF = LineOver[Float3, Float]
  object LineF{
    def apply(start: Float3, end: Float3) = LineOver[Float3, Float](start, end)
  }
  type OBBF = OBBOver[Float3,Float]
  object OBBF{
    def apply(center: Float3, right: Float3, up: Float3, extentRight: Float, extentUp: Float, extentLook: Float) = new OBBOver(center, right, up, extentRight, extentUp, extentLook)
  }
  type PlaneF = PlaneOver[Float3, Float]
  object PlaneF{
    def apply(point: Float3, normal: Float3) = PlaneOver[Float3, Float](point, normal)
  }
  type Ray2F = Ray2Over[Float2, Float]
  object Ray2F{
    def apply(start: Float2, end: Float2) = Ray2Over[Float2,Float](start, end)
  }
  type RayF = RayOver[Float3, Float]
  object RayF{
    def apply(start: Float3, end: Float3) = RayOver[Float3, Float](start, end)
  }
  type Rectangle2F = Rectangle2Over[Float2, Float]
  object Rectangle2F{
    def apply(center: Float2, extent: Float2) = Rectangle2Over[Float2,Float](center, extent)
  }
  type RectangleF = RectangleOver[Float3, Float]
  object RectangleF{
    def apply(center: Float3, right : Float3, up : Float3) = RectangleOver[Float3, Float](center, right, up)
  }
  type SphereF = SphereOver[Float3, Float]
  object SphereF{
    def apply(center: Float3, rad: Float) = SphereOver[Float3, Float](center, rad)
  }
  type Square2F = Square2Over[Float2, Float]
  object Square2F{
    def apply(center: Float2, extent: Float) = Square2Over[Float2, Float](center, extent)
  }
  type TriangleF = TriangleOver[Float3, Float]
  object TriangleF{
    def apply(p1: Float3, p2: Float3, p3: Float3) = TriangleOver[Float3, Float](p1, p2, p3)
  }
  type OBB2F = OBB2Over[Float2, Float]
  object OBB2F{
    def apply(center: Float2, right: Float2, up: Float2, extentRight: Float, extentUp: Float): OBB2F = OBB2Over(center, right, up, extentRight, extentUp)
  }
  //--------------------------------------------------------
  
  
  
  
  
  

  //common algebraic structures-----------------------------
  type Reals = Field[Real]
  type V4 = CanonicalEuclideanSpaceOverField[Real4, Real, Nat._4]
  type V3 = CanonicalEuclideanSpaceOverField[Real3, Real, Nat._3]
  type V2 = CanonicalEuclideanSpaceOverField[Real2, Real, Nat._2]

  type I2 = ModuleOverRing[Int2, Int, Nat._2]
  type I3 = ModuleOverRing[Int3, Int, Nat._3]
  type I4 = ModuleOverRing[Int4, Int, Nat._4]


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
    import Russoul.lib.common.TypeClasses.DoubleIsFullField._
    @inline def apply(seq: Real*):Vec[Real]  = {
      val ar = new Array[Real](seq.size)
      var i = 0
      while(i < ar.size){
        ar(i) = seq(i)
        i += 1
      }

      Vec(ar)
    }
  }

  //same as RealN
  object Real{
    import Russoul.lib.common.TypeClasses.DoubleIsFullField._
    @inline def apply(seq: Real*):Vec[Real]  = {
      val ar = new Array[Real](seq.size)
      var i = 0
      while(i < ar.size){
        ar(i) = seq(i)
        i += 1
      }

      Vec(ar)
    }
  }

  object Double2{
    @inline def apply(x: Double, y: Double) = Vec2(x,y)
  }

  object Double3{
    @inline def apply(x: Double, y: Double, z: Double) = Vec3(x,y,z)
    @inline def apply(v2:Vec2[Double], z:Double) = Vec3(v2.x, v2.y, z)
  }

  object Double4{
    @inline def apply(x: Double, y: Double, z: Double, w: Double)  = Vec4(x,y,z,w)
    @inline def apply(v:Vec3[Double], w:Double): Vec4[Double] = Vec4(v.x, v.y, v.z, w)
  }

  object DoubleN{
    import Russoul.lib.common.TypeClasses.DoubleIsFullField._
    @inline def apply(seq: Double*):Vec[Double]  = {
      val ar = new Array[Double](seq.size)
      var i = 0
      while(i < ar.size){
        ar(i) = seq(i)
        i += 1
      }

      Vec(ar)
    }
  }

  object Float2{
    @inline def apply(x: Float, y: Float) = Vec2(x,y)
  }

  object Float3{
    @inline def apply(x: Float, y: Float, z: Float) = Vec3(x,y,z)
    @inline def apply(v2:Vec2[Float], z:Float) = Vec3(v2.x, v2.y, z)
  }

  object Float4{
    @inline def apply(x: Float, y: Float, z: Float, w: Float)  = Vec4(x,y,z,w)
    @inline def apply(v:Vec3[Float], w:Float): Vec4[Float] = Vec4(v.x, v.y, v.z, w)
  }


  object Real2F{
    @inline def apply(x: Float, y: Float) = Vec2(x,y)
  }

  object Real3F{
    @inline def apply(x: Float, y: Float, z: Float) = Vec3(x,y,z)
    @inline def apply(v2:Vec2[Float], z:Float) = Vec3(v2.x, v2.y, z)
  }

  object Real4F{
    @inline def apply(x: Float, y: Float, z: Float, w: Float)  = Vec4(x,y,z,w)
    @inline def apply(v:Vec3[Float], w:Float): Vec4[Float] = Vec4(v.x, v.y, v.z, w)
  }

  object FloatN{
    @inline def apply(seq: Float*):Vec[Float]  = {
      val ar = new Array[Float](seq.size)
      var i = 0
      while(i < ar.size){
        ar(i) = seq(i)
        i += 1
      }

      Vec(ar)
    }
  }

  object Int2{
    @inline def apply(x: Int, y: Int) = Vec2(x,y)
  }

  object Int3{
    @inline def apply(x: Int, y: Int, z: Int) = Vec3(x,y,z)
    @inline def apply(v2:Vec2[Int], z:Int) = Vec3(v2.x, v2.y, z)
  }

  object Int4{
    @inline def apply(x: Int, y: Int, z: Int, w: Int)  = Vec4(x,y,z,w)
    @inline def apply(v:Vec3[Int], w:Int): Vec4[Int] = Vec4(v.x, v.y, v.z, w)
  }
  


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
    * as you see this function does not perform common sense checks like bounds check and check for
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
    implicit object NotNothing extends NotNothing[Any]
  }




}

package russoul.lib


import russoul.lib.common.Abstraction._
import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.geometry.simple._
import russoul.lib.common.Implicits._
import russoul.lib.common.math.algebra.{ComplexOver, Mat, Vec}
import shapeless.Nat
import shapeless.Nat._
import shapeless.ops.nat.ToInt

import scala.annotation.Annotation
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}


/**
  * Created by russoul on 11.05.17.
  */
package object common
{


  //Functional

  //TODO this currently does not work with functions that take implicit parameters, fixed in dotty ?

  implicit class PipeVarags[A,R](val a: Seq[A]) extends AnyVal{ //AnyVal actually works ?
    def |*>(f: (A*) => R): R = f(a : _*)
  }

  implicit class Pipe1[A,R](val a: A) extends AnyVal{
    def |>(f: A => R): R = f(a)
  }

  //TODO
  implicit class Pipe2[A,B,R](val a: (A,B)) extends AnyVal{
    def ||>(f: (A,B) => R): R = f(a._1,a._2)
  }

  implicit class Pipe3[A,B,C,R](val a: (A,B,C)) extends AnyVal{
    def |||>(f: (A,B,C) => R): R = f(a._1,a._2,a._3)
  }

  implicit class Pipe4[A,B,C,D,R](val a: (A,B,C,D)) extends AnyVal{
    def ||||>(f: (A,B,C,D) => R): R = f(a._1,a._2,a._3,a._4)
  }
  //TODO............

  implicit class FuncApply[A,B](val f : A => B) extends AnyVal{
    def $(a : A) : B = f(a)
  }



  //............................................


  //Functional mathematical Shape 2D


  //TODO better design FShape2, it is too verbose
  trait FShape2[@tbsp A]{ self =>
    def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec,A,_2], t1: Tensor1[A,Vec,_2]) : A

    def &(that: FShape2[A])(implicit order : Orderable[A]) : FShape2[A] = {
      new FShape2[A] {
        override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
          order.max(self.density(p), that.density(p))
        }
      }
    }

    def |(that: FShape2[A])(implicit order : Orderable[A]) : FShape2[A] = {
      new FShape2[A] {
        override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
          order.min(self.density(p), that.density(p))
        }
      }
    }

    def -(that: FShape2[A])(implicit order : Orderable[A]) : FShape2[A] = {
      new FShape2[A] {
        override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
          order.max(self.density(p), -that.density(p))
        }
      }
    }
  }

  case class FCircle[@tbsp A](center : Vec2[A], rad: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
      val d = p - center
      (d dot d) - rad * rad
    }
  }

  case class FHalfPlaneLeft[@tbsp A](x: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
      p.x - x
    }
  }

  case class FHalfPlaneRight[@tbsp A](x: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
      x - p.x
    }
  }

  case class FHalfPlaneUpper[@tbsp A](y: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
      y - p.y
    }
  }

  case class FHalfPlaneLower[@tbsp A](y: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
      p.y - y
    }
  }

  case class FRectangle2[@tbsp A](center : Vec2[A], extent: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2], order: Orderable[A]) extends FShape2[A]{

    val shape = FHalfPlaneRight(center.x - extent.x) & FHalfPlaneLeft(center.x + extent.x) &
      FHalfPlaneLower(center.y + extent.y) & FHalfPlaneUpper(center.y - extent.y)

    override def density(p: Vec2[A])(implicit field: Field[A], ces: CES[Vec, A, _2], t1: Tensor1[A,Vec,_2]): A = {
      shape.density(p)
    }
  }

  //..........


  //try with resources

  def cleanly[A, B](resource: A)(cleanup: A => Unit)(doWork: A => B): Try[B] = {
    try {
      Success(doWork(resource))
    } catch {
      case e: Exception => Failure(e)
    }
    finally {
      try {
        if (resource != null) {
          cleanup(resource)
        }
      } catch {
        case e: Exception => println(e) // should be logged
      }
    }
  }

  def autoTry[A <: AutoCloseable, B](resource: A)(doWork: A => B): Try[B] = {
    try {
      Success(doWork(resource))
    } catch {
      case e: Exception => Failure(e)
    }
    finally {
      try {
        if (resource != null) {
          resource.close()
        }
      } catch {
        case e: Exception => println(e) // should be logged
      }
    }
  }

  def auto[A <: AutoCloseable, B](resource: A)(doWork: A => B): B = {
    try{
      doWork(resource)
    } finally {
      resource.close()
    }
  }

  //specialization
  type sp = specialized
  class tbsp extends Annotation //tbsp for to be specialized; currently specialization does not work well
  //....

  //simple types---------------------------
  //Real is just another name for double, it must not be changed to float or any other similar types(there is actually no other similar types on JVM)
  type Real = Double //TODO changing this leads to errors, so dont !
  type Complex = ComplexOver[Real]
  type Real2 = Vec[Real,Nat._2]
  type Real3 = Vec[Real,Nat._3]
  type Real4 = Vec[Real,Nat._4]
  //type RealN = Vec[Real]

  type RealF = Float
  type RealD = Double
  type Real2F = Float2
  type Real3F = Float3
  type Real4F = Float4
  type Real2D = Real2
  type Real3D = Real3
  type Real4D = Real4

  type ComplexF = ComplexOver[Float]

  type Vec2[@tbsp T] = Vec[T, Nat._2]
  type Vec3[@tbsp T] = Vec[T, Nat._3]
  type Vec4[@tbsp T] = Vec[T, Nat._4]

  type Mat2[@tbsp A] = Mat[A, Nat._2, Nat._2]
  type Mat3[@tbsp A] = Mat[A, Nat._3, Nat._3]
  type Mat4[@tbsp A] = Mat[A, Nat._4, Nat._4]


  object Vec2{
    @inline def apply[@tbsp A : ClassTag](x: A, y: A): Vec[A, Nat._2] = Vec[A,Nat._2](x,y)
  }
  object Vec3{
    @inline def apply[@tbsp A : ClassTag](x: A, y: A, z: A): Vec[A, Nat._3] = Vec[A,Nat._3](x,y,z)
  }
  object Vec4{
    @inline def apply[@tbsp A : ClassTag](x: A, y: A, z: A, w: A): Vec[A, Nat._4] = Vec[A,Nat._4](x,y,z,w)
  }

  //The same as Real
  type Double2 = Real2
  type Double3 = Real3
  type Double4 = Real4
  //...................

  type Float2 = Vec[Float, Nat._2]
  type Float3 = Vec[Float, Nat._3]
  type Float4 = Vec[Float, Nat._4]

  type Int2 = Vec[Int, Nat._2]
  type Int3 = Vec[Int, Nat._3]
  type Int4 = Vec[Int, Nat._4]


  type Mat4D = Mat4[Double]
  type Mat4F = Mat4[Float]

  //--------------------------------------

  //Common simple geometric objects over reals--------------
  type AABB = AABBOver[Vec, Real]
  object AABB{
    def apply(center: Real3, extent: Real3) = AABBOver[Vec,Real](center, extent)
  }
  type Circle = CircleOver[Vec, Real]
  object Circle{
    def apply(center : Real2, rad: Real) = CircleOver[Vec,Real](center, rad)
  }
  type Line2 = Line2Over[Vec, Real]
  object Line2{
    def apply(start : Real2, end : Real2): Line2 = Line2Over[Vec, Real](start, end)
  }
  type Line = LineOver[Vec, Real]
  object Line{
    def apply(start: Real3, end: Real3): Line = LineOver[Vec, Real](start, end)
  }
  type OBB = OBBOverReal
  object OBB{
    def apply(center: Real3, right: Real3, up: Real3, extentRight: Real, extentUp: Real, extentLook: Real): OBB = OBBOverReal(center, right, up, extentRight, extentUp, extentLook)
  }
  type Plane = PlaneOver[Vec, Real]
  object Plane{
    def apply(point: Real3, normal: Real3): Plane = PlaneOver[Vec, Real](point, normal)
  }
  type Ray2 = Ray2Over[Vec, Real]
  object Ray2{
    def apply(start: Real2, dir: Real2): Ray2 = Ray2Over[Vec,Real](start, dir)
  }
  type Ray = RayOver[Vec, Real]
  object Ray{
    def apply(start: Real3, dir: Real3): Ray = RayOver[Vec, Real](start, dir)
  }
  type Rectangle2 = Rectangle2Over[Vec, Real]
  object Rectangle2{
    def apply(center: Real2, extent: Real2): Rectangle2 = Rectangle2Over[Vec,Real](center, extent)
  }
  type Rectangle = RectangleOver[Vec, Real]
  object Rectangle{
    def apply(center: Real3, right : Real3, up : Real3): Rectangle = RectangleOver[Vec, Real](center, right, up)
  }
  type Sphere = SphereOver[Vec, Real]
  object Sphere{
    def apply(center: Real3, rad: Real): Sphere = SphereOver[Vec, Real](center, rad)
  }
  type Square2 = Square2Over[Vec, Real]
  object Square2{
    def apply(center: Real2, extent: Real): Square2 = Square2Over(center, extent)
  }
  type Triangle = TriangleOver[Vec, Real]
  object Triangle{
    def apply(p1: Real3, p2: Real3, p3: Real3): TriangleOver[Vec,Real] = TriangleOver(p1, p2, p3)
  }
  type OBB2 = OBB2Over[Vec, Real]
  object OBB2{
    def apply(center: Real2, right: Real2, up: Real2, extentRight: Real, extentUp: Real): OBB2Over[Vec, Real] = OBB2Over(center, right, up, extentRight, extentUp)
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
  type AABBF = AABBOver[Vec, Float]
  object AABBF{
    def apply(center: Float3, extent: Float3) = AABBOver[Vec, Float](center, extent)
  }
  type CircleF = CircleOver[Vec, Float]
  object CircleF{
    def apply(center : Float2, rad: Float) = CircleOver(center, rad)
  }
  type Line2F = Line2Over[Vec, Float]
  object Line2F{
    def apply(start : Float2, end : Float2) = Line2Over[Vec, Float](start, end)
  }
  type LineF = LineOver[Vec, Float]
  object LineF{
    def apply(start: Float3, end: Float3) = LineOver[Vec, Float](start, end)
  }
  type OBBF = OBBOver[Vec,Float]
  object OBBF{
    def apply(center: Float3, right: Float3, up: Float3, extentRight: Float, extentUp: Float, extentLook: Float) = OBBOver(center, right, up, extentRight, extentUp, extentLook)
  }
  type PlaneF = PlaneOver[Vec, Float]
  object PlaneF{
    def apply(point: Float3, normal: Float3) = PlaneOver[Vec, Float](point, normal)
  }
  type Ray2F = Ray2Over[Vec, Float]
  object Ray2F{
    def apply(start: Float2, end: Float2) = Ray2Over[Vec,Float](start, end)
  }
  type RayF = RayOver[Vec, Float]
  object RayF{
    def apply(start: Float3, end: Float3) = RayOver[Vec, Float](start, end)
  }
  type Rectangle2F = Rectangle2Over[Vec, Float]
  object Rectangle2F{
    def apply(center: Float2, extent: Float2) = Rectangle2Over[Vec,Float](center, extent)
  }
  type RectangleF = RectangleOver[Vec, Float]
  object RectangleF{
    def apply(center: Float3, right : Float3, up : Float3) = RectangleOver[Vec, Float](center, right, up)
  }
  type SphereF = SphereOver[Vec, Float]
  object SphereF{
    def apply(center: Float3, rad: Float) = SphereOver[Vec, Float](center, rad)
  }
  type Square2F = Square2Over[Vec, Float]
  object Square2F{
    def apply(center: Float2, extent: Float) = Square2Over[Vec, Float](center, extent)
  }
  type TriangleF = TriangleOver[Vec, Float]
  object TriangleF{
    def apply(p1: Float3, p2: Float3, p3: Float3) = TriangleOver[Vec, Float](p1, p2, p3)
  }
  type OBB2F = OBB2Over[Vec, Float]
  object OBB2F{
    def apply(center: Float2, right: Float2, up: Float2, extentRight: Float, extentUp: Float): OBB2F = OBB2Over(center, right, up, extentRight, extentUp)
  }
  type Triangle2F = Triangle2Over[Vec, Float]
  object Triangle2F{
    def apply(p1: Float2, p2: Float2, p3: Float2) = Triangle2Over[Vec,Float](p1,p2,p3)
  }
  //--------------------------------------------------------


  //SOME SYNTACTIC GOODIES
  //used as fully infered function
  @inline def makeVector[Dim <: Nat, Vec[_,_ <: Nat], @tbsp F](dim : Dim, args: F*)(implicit ev: CanonicalEuclideanSpaceOverField[Vec, F, Dim], toInt: ToInt[Dim]) = ev.tensor1.make(args : _*)
  @inline def transformd(a: Real3, b: Mat4D) : Real3 = {
    val temp = Real4(a, 1D)
    val temp2 = temp * b
    Real3(temp2.x, temp2.y, temp2.z)
  }
  @inline def transformf(a: Real3F, b: Mat4F) : Real3F = {
    val temp = Real4F(a, 1F)
    val temp2 = temp * b
    Real3F(temp2.x, temp2.y, temp2.z)
  }

  //........................
  

  //common algebraic structures-----------------------------
  type Reals = Field[Real]
  type V4 = CanonicalEuclideanSpaceOverField[Vec, Real, Nat._4]
  type V3 = CanonicalEuclideanSpaceOverField[Vec, Real, Nat._3]
  type V2 = CanonicalEuclideanSpaceOverField[Vec, Real, Nat._2]

  type R2 = Module[Vec, Int, Nat._2]
  type R3 = Module[Vec, Int, Nat._3]
  type R4 = Module[Vec, Int, Nat._4]


  object Real2{
    @inline def apply(x: Real, y: Real) = Vec[Real, _2](x,y)
  }
  
  object Real3{
    @inline def apply(x: Real, y: Real, z: Real) = Vec[Real, _3](x,y,z)
    @inline def apply(v2:Real2, z:Real) = Vec[Real, _3](v2.x, v2.y, z)
  }
  
  object Real4{
    @inline def apply(x: Real, y: Real, z: Real, w: Real) = Vec[Real,_4](x,y,z,w)
    @inline def apply(v:Real3, w:Real): Real4 = Vec[Real, _4](v.x, v.y, v.z, w)
  }


  object Double2{
    @inline def apply(x: Double, y: Double) = Vec[Double, _2](x,y)
  }

  object Double3{
    @inline def apply(x: Double, y: Double, z: Double) = Vec[Double, _3](x,y,z)
    @inline def apply(v2:Double2, z:Double) = Vec[Double, _3](v2.x, v2.y, z)
  }

  object Double4{
    @inline def apply(x: Double, y: Double, z: Double, w: Double)  = Vec[Double, _4](x,y,z,w)
    @inline def apply(v:Double3, w:Double): Double4 = Vec[Double, _4](v.x, v.y, v.z, w)
  }


  object Float2{
    @inline def apply(x: Float, y: Float) = Vec[Float, _2](x,y)
  }

  object Float3{
    @inline def apply(x: Float, y: Float, z: Float) = Vec[Float, _3](x,y,z)
    @inline def apply(v2:Float2, z:Float) = Vec[Float, _3](v2.x, v2.y, z)
  }

  object Float4{
    @inline def apply(x: Float, y: Float, z: Float, w: Float)  = Vec[Float, _4](x,y,z,w)
    @inline def apply(v:Float3, w:Float): Float4 = Vec[Float, _4](v.x, v.y, v.z, w)
  }


  object Real2F{
    @inline def apply(x: Float, y: Float) = Vec[RealF, _2](x,y)
  }

  object Real3F{
    @inline def apply(x: Float, y: Float, z: Float) = Vec[RealF, _3](x,y,z)
    @inline def apply(v2:Float2, z:Float) = Vec[RealF, _3](v2.x, v2.y, z)
  }

  object Real4F{
    @inline def apply(x: Float, y: Float, z: Float, w: Float)  = Vec[RealF, _4](x,y,z,w)
    @inline def apply(v:Float3, w:Float): Float4 = Vec[RealF, _4](v.x, v.y, v.z, w)
  }


  object Int2{
    @inline def apply(x: Int, y: Int) = Vec[Int, _2](x,y)
  }

  object Int3{
    @inline def apply(x: Int, y: Int, z: Int) = Vec[Int, _3](x,y,z)
    @inline def apply(v2:Int2, z:Int) = Vec[Int, _3](v2._0, v2._1, z)
  }

  object Int4{
    @inline def apply(x: Int, y: Int, z: Int, w: Int)  = Vec[Int, _4](x,y,z,w)
    @inline def apply(v:Int3, w:Int): Int4 = Vec[Int, _4](v._0, v._1, v._2, w)
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



  object Mat4F{

    //OPENGL uses column-major form, DirectX - row major

    //common transformations for V₃ over Floats------------------------------------------
    def identity(): Mat4F =
    {



      Mat[Float, Nat._4](
        1F, 0F, 0F, 0F,
        0F, 1F, 0F, 0F,
        0F, 0F, 1F, 0F,
        0F, 0F, 0F, 1F)
    }

    def scale(x: Float, y: Float, z: Float): Mat4F =
    {
      Mat[Float, Nat._4](
        x, 0F, 0F, 0F,
        0F, y, 0F, 0F,
        0F, 0F, z, 0F,
        0F, 0F, 0F, 1F)
    }

    def scale(v: Float3): Mat4F =
    {
      Mat[Float,Nat._4](
        v.x, 0, 0, 0,
        0, v.y, 0, 0,
        0, 0, v.z, 0,
        0, 0, 0, 1)
    }

    def translation(x: Float, y: Float, z: Float): Mat4F =
    {
      Mat[Float,Nat._4](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        x, y, z, 1)
    }

    def translation(v: Float3): Mat4F =
    {
      Mat[Float,Nat._4](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        v.x, v.y, v.z, 1)
    }

    def rotationDeg(axis: Float3, angleInDegrees: Float): Mat4F =
    {
      val rad = angleInDegrees * scala.math.Pi / 180
      val cos = scala.math.cos(rad).toFloat
      val sin = scala.math.sin(rad).toFloat
      val x = axis.x
      val y = axis.y
      val z = axis.z
      Mat[Float,Nat._4](
        cos + x * x * (1 - cos), x * y * (1 - cos) - z * sin, x * z * (1 - cos) + y * sin, 0,
        y * x * (1 - cos) + z * sin, cos + y * y * (1 - cos), y * z * (1 - cos) - x * sin, 0,
        z * x * (1 - cos) - y * sin, z * y * (1 - cos) + x * sin, cos + z * z * (1 - cos), 0,
        0, 0, 0, 1)

    }

    def rotationRad(axis: Float3, angleInRadians: Float): Mat4F =
    {

      val cos = Math.cos(angleInRadians).toFloat
      val sin = Math.sin(angleInRadians).toFloat
      val x = axis.x * axis.x
      val y = axis.y * axis.y
      val z = axis.z * axis.z
      Mat[Float,Nat._4](
        cos + x * x * (1F - cos), x * y * (1F - cos) - z * sin, x * z * (1F - cos) + y * sin, 0F,
        y * x * (1F - cos) + z * sin, cos + y * y * (1F - cos), y * z * (1F - cos) - x * sin, 0F,
        z * x * (1F - cos) - y * sin, z * y * (1F - cos) + x * sin, cos + z * z * (1F - cos), 0F,
        0F, 0F, 0F, 1F)

    }

    def ortho(left: Float, right: Float, bottom: Float, top: Float, near: Float, far: Float): Mat4F =
    {

      Mat[Float, Nat._4](
        2 / (right - left), 0, 0, -(right + left) / (right - left),
        0, 2 / (top - bottom), 0, -(top + bottom) / (top - bottom),
        0, 0, -2 / (far - near), -(far + near) / (far - near),
        0, 0, 0, 1)
    }


    def perspective(angleInFegrees: Float, aspect: Float, near: Float, far: Float): Mat4F =
    {
      val top:Float = near * scala.math.tan(scala.math.Pi / 180 * angleInFegrees / 2).toFloat
      val bottom = -top
      val right = top * aspect
      val left = -right

      Mat[Float,Nat._4](
        2 * near / (right - left), 0, (right + left) / (right - left), 0, //OpenGL form(column-major) not transposed, transposed - row-major form
        0, 2 * near / (top - bottom), (top + bottom) / (top - bottom), 0,
        0, 0, -(far + near) / (far - near), -2 * (far * near) / (far - near),
        0, 0, -1, 0)
    }




    def view(pos: Float3, target: Float3, up: Float3): Mat4F =
    {

      val za = (target - pos).normalize()
      val xa = up.⨯(za).normalize()
      val ya = za.⨯(xa)
      Mat[Float,Nat._4](
        xa.x, ya.x, za.x, 0F,
        xa.y, ya.y, za.y, 0F,
        xa.z, ya.z, za.z, 0F,
        -xa.⋅(pos), -ya.⋅(pos), -za.⋅(pos), 1F)
    }

    def viewDir(pos: Vec3[Float], look: Vec3[Float], up: Vec3[Float]): Mat4F =
    {


      val za = -look
      val xa = up.⨯(za).normalize()
      val ya = za.⨯(xa)
      Mat[Float,Nat._4](
        xa.x, ya.x, za.x, 0F,
        xa.y, ya.y, za.y, 0F,
        xa.z, ya.z, za.z, 0F,
        -xa.⋅(pos), -ya.⋅(pos), -za.⋅(pos), 1F)
    }

    def view(pos: Vec3[Float], rotX: Float, rotY: Float, rotZ: Float): Mat4F =
    {

      var mat = identity()
      mat ⨯= translation(-pos)
      mat ⨯= rotationDeg(Vec3(0F, 1F, 0F), rotY)
      mat ⨯= rotationDeg(Vec3(1F, 0F, 0F), rotX)
      mat ⨯= rotationDeg(Vec3(0F, 0F, 1F), rotZ)
      mat
    }
  }

  object Mat4D
  {


    //common transformations for V₃ over Real------------------------------------------
    def identity(): Mat4D = //row major
    {

      Mat[Double,Nat._4](
        1D, 0D, 0D, 0D,
        0D, 1D, 0D, 0D,
        0D, 0D, 1D, 0D,
        0D, 0D, 0D, 1D)
    }

    def scale(x: Real, y: Real, z: Real): Mat4D = //row major
    {
      Mat[Double,Nat._4](
        x, 0D, 0D, 0D,
        0D, y, 0D, 0D,
        0D, 0D, z, 0D,
        0D, 0D, 0D, 1D)
    }

    def scale(v: Vec3[Real]): Mat4D = //row major
    {
      Mat[Double,Nat._4](
        v.x, 0, 0, 0,
        0, v.y, 0, 0,
        0, 0, v.z, 0,
        0, 0, 0, 1)
    }

    def translation(x: Real, y: Real, z: Real): Mat4D = //row major
    {
      Mat[Double,Nat._4](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        x, y, z, 1)
    }

    def translation(v: Vec3[Real]): Mat4D = //row major
    {
      Mat[Double,Nat._4](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        v.x, v.y, v.z, 1)
    }

    def rotationDeg(axis: Vec3[Real], angleInDegrees: Real): Mat4D = //row major
    {
      val rad = angleInDegrees * scala.math.Pi / 180
      val cos = scala.math.cos(rad)
      val sin = scala.math.sin(rad)
      val x = axis.x
      val y = axis.y
      val z = axis.z
      Mat[Real,_4](
        cos + x * x * (1 - cos), x * y * (1 - cos) - z * sin, x * z * (1 - cos) + y * sin, 0,
        y * x * (1 - cos) + z * sin, cos + y * y * (1 - cos), y * z * (1 - cos) - x * sin, 0,
        z * x * (1 - cos) - y * sin, z * y * (1 - cos) + x * sin, cos + z * z * (1 - cos), 0,
        0, 0, 0, 1)

    }

    def rotationRad(axis: Vec3[Real], angleInRadians: Real): Mat4D = //row major
    {

      val cos = scala.math.cos(angleInRadians)
      val sin = scala.math.sin(angleInRadians)
      val x = axis.x * axis.x
      val y = axis.y * axis.y
      val z = axis.z * axis.z
      Mat[Double,Nat._4](
        cos + x * x * (1D - cos), x * y * (1D - cos) - z * sin, x * z * (1D - cos) + y * sin, 0D,
        y * x * (1D - cos) + z * sin, cos + y * y * (1D - cos), y * z * (1D - cos) - x * sin, 0D,
        z * x * (1D - cos) - y * sin, z * y * (1D - cos) + x * sin, cos + z * z * (1D - cos), 0D,
        0D, 0D, 0D, 1D)

    }

    def ortho(left: Real, right: Real, bottom: Real, top: Real, near: Real, far: Real): Mat4D = //column major
    {

      Mat[Real,_4](
        2 / (right - left), 0, 0, -(right + left) / (right - left),
        0, 2 / (top - bottom), 0, -(top + bottom) / (top - bottom),
        0, 0, -2 / (far - near), -(far + near) / (far - near),
        0, 0, 0, 1)
    }


    def perspective(angleInDegrees: Real, aspect: Real, near: Real, far: Real): Mat4D = //column major
    {
      val top:Real = near * scala.math.tan(scala.math.Pi / 180 * angleInDegrees / 2)
      val bottom = -top
      val right = top * aspect
      val left = -right

      Mat[Real,_4](
        2 * near / (right - left), 0D, (right + left) / (right - left), 0D,
        0D, 2 * near / (top - bottom), (top + bottom) / (top - bottom), 0D,
        0, 0, -(far + near) / (far - near), -2 * (far * near) / (far - near),
        0, 0, -1, 0)
    }



    def view(pos: Vec3[Real], target: Vec3[Real], up: Vec3[Real]): Mat4D = //row major?
    {

      val za = (target - pos).normalize()
      val xa = up.⨯(za).normalize()
      val ya = za.⨯(xa)
      Mat[Real,_4](
        xa.x, ya.x, za.x, 0D,
        xa.y, ya.y, za.y, 0D,
        xa.z, ya.z, za.z, 0D,
        -xa.⋅(pos), -ya.⋅(pos), -za.⋅(pos), 1D)
    }

    def viewDir(pos: Vec3[Real], look: Vec3[Real], up: Vec3[Real]): Mat4D = //row major?
    {


      val za = -look
      val xa = up.⨯(za).normalize()
      val ya = za.⨯(xa)
      Mat[Double,Nat._4](
        xa.x, ya.x, za.x, 0D,
        xa.y, ya.y, za.y, 0D,
        xa.z, ya.z, za.z, 0D,
        -xa.⋅(pos), -ya.⋅(pos), -za.⋅(pos), 1D)
    }

    def view(pos: Vec3[Real], rotX: Real, rotY: Real, rotZ: Real): Mat4D = //row major?
    {

      var mat = identity()
      mat ⨯= translation(-pos)
      mat ⨯= rotationDeg(Vec3(0D, 1D, 0D), rotY)
      mat ⨯= rotationDeg(Vec3(1D, 0D, 0D), rotX)
      mat ⨯= rotationDeg(Vec3(0D, 0D, 1D), rotZ)
      mat
    }
  }


}

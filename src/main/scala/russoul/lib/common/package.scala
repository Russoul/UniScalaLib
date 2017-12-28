package russoul.lib


import russoul.lib.common.math.geometry.simple._
import russoul.lib.common.math.algebra.{ComplexOver, Mat, Row}
import spire.algebra.{Field, Order}

import scala.annotation.Annotation
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import russoul.lib.common.Implicits._
import singleton.ops.XInt
import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 11.05.17.
  */
package object common
{


  //Functional


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

  implicit class FuncApply1[A,B](val f : A => B) extends AnyVal{
    def +>(a : A) : B = f(a)
  }
  implicit class FuncApply2[A,B,C](val f : A => B => C) extends AnyVal{
    def +>(a : A) : B => C = f(a)
  }
  /*implicit class FuncApply3[A,B,C,D](val f : A => B => C => D) extends AnyVal{
    def $(a : A) : B => C => D = f(a)
  }
  implicit class FuncApply4[A,B,C,D,E](val f : A => B => C => D => E) extends AnyVal{
    def $(a : A) : B => C => D => E = f(a)
  }
  implicit class FuncApply5[A,B,C,D,E,F](val f : A => B => C => D => E => F) extends AnyVal{
    def $(a : A) : B => C => D => E => F = f(a)
  }*/
  //............................................


  //Functional mathematical Shape 2D


  //TODO better design FShape2, it is too verbose
  trait FShape2[@specialized(Float,Double,Int) A]{ self =>
    def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]) : A

    def &(that: FShape2[A])(implicit order : Order[A]) : FShape2[A] = {
      new FShape2[A] {
        override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
          order.max(self.density(p), that.density(p))
        }
      }
    }

    def |(that: FShape2[A])(implicit order : Order[A]) : FShape2[A] = {
      new FShape2[A] {
        override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
          order.min(self.density(p), that.density(p))
        }
      }
    }

    def -(that: FShape2[A])(implicit order : Order[A]) : FShape2[A] = {
      new FShape2[A] {
        override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
          order.max(self.density(p), -that.density(p))
        }
      }
    }
  }

  case class FCircle[@specialized(Float,Double,Int) A](center : Vec2[A], rad: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
      val d = p - center
      (d dot d) - rad * rad
    }
  }

  case class FHalfPlaneLeft[@specialized(Float,Double,Int) A](x: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
      p(0) - x
    }
  }

  case class FHalfPlaneRight[@specialized(Float,Double,Int) A](x: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
      x - p(0)
    }
  }

  case class FHalfPlaneUpper[@specialized(Float,Double,Int) A](y: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
      y - p(1)
    }
  }

  case class FHalfPlaneLower[@specialized(Float,Double,Int) A](y: A) extends FShape2[A]{
    override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
      p(1) - y
    }
  }

  case class FRectangle2[@specialized(Float,Double,Int) A](center : Vec2[A], extent: Vec2[A])(implicit field: Field[A], order: Order[A]) extends FShape2[A]{

    val shape = FHalfPlaneRight(center(0) - extent(0)) & FHalfPlaneLeft(center(0) + extent(0)) &
      FHalfPlaneLower(center(1) + extent(1)) & FHalfPlaneUpper(center(1) - extent(1))

    override def density(p: Vec2[A])(implicit field: Field[A], tag : ClassTag[A]): A = {
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
  //type sp = specialized
  class tbsp extends Annotation //tbsp for to be specialized; currently specialization does not work well
  //....


  type _1 = 1
  type _2 = 2
  type _3 = 3
  type _4 = 4
  
  //simple types---------------------------

  type ComplexD = ComplexOver[Double]
  type ComplexF = ComplexOver[Float]


  type Row[@specialized(Float,Double,Int) T, Size <: XInt] = Mat[T,_1,Size]
  type Column[@specialized(Float,Double,Int) T, Size <: XInt] = Mat[T, Size, _1] //TODO

  type Vec2[@specialized(Float,Double,Int) T] = Row[T, _2]
  type Vec3[@specialized(Float,Double,Int) T] = Row[T, _3]
  type Vec4[@specialized(Float,Double,Int) T] = Row[T, _4]

  type Mat2[@specialized(Float,Double,Int) A] = Mat[A, _3, _3]
  type Mat3[@specialized(Float,Double,Int) A] = Mat[A, _3, _3]
  type Mat4[@specialized(Float,Double,Int) A] = Mat[A, _4, _4]


  object Vec2{
    @inline def apply[@specialized(Float,Double,Int) A : ClassTag](x: A, y: A): Row[A, _2] = Row[A,_2](x,y)
  }
  object Vec3{
    @inline def apply[@specialized(Float,Double,Int) A : ClassTag](x: A, y: A, z: A): Row[A, _3] = Row[A,_3](x,y,z)
  }
  object Vec4{
    @inline def apply[@specialized(Float,Double,Int) A : ClassTag](x: A, y: A, z: A, w: A): Row[A, _4] = Row[A,_4](x,y,z,w)
  }

  type Double2 = Row[Double,_2]
  type Double3 = Row[Double,_3]
  type Double4 = Row[Double,_4]
  //...................

  type Float2 = Row[Float, _2]
  type Float3 = Row[Float, _3]
  type Float4 = Row[Float, _4]

  type Int2 = Row[Int, _2]
  type Int3 = Row[Int, _3]
  type Int4 = Row[Int, _4]



  type Mat4D = Mat4[Double]
  type Mat4F = Mat4[Float]

  //--------------------------------------

  //Common simple geometric objects over Doubles--------------
  type AABB = AABBOver[Double]
  object AABB{
    def apply(center: Row[Double,_3], extent: Row[Double,_3]) = AABBOver[Double](center, extent)
  }
  type Circle = CircleOver[Double]
  object Circle{
    def apply(center : Row[Double,_2], rad: Double) = CircleOver[Double](center, rad)
  }
  type Line2 = Line2Over[Double]
  object Line2{
    def apply(start : Double2, end : Double2): Line2 = Line2Over[Double](start, end)
  }
  type Line = LineOver[Double]
  object Line{
    def apply(start: Double3, end: Double3): Line = LineOver[Double](start, end)
  }
  type OBB = OBBOverDouble
  object OBB{
    def apply(center: Double3, right: Double3, up: Double3, extentRight: Double, extentUp: Double, extentLook: Double): OBB = OBBOverDouble(center, right, up, extentRight, extentUp, extentLook)
  }
  type Plane = PlaneOver[Double]
  object Plane{
    def apply(point: Double3, normal: Double3): Plane = PlaneOver[Double](point, normal)
  }
  type Ray2 = Ray2Over[Double]
  object Ray2{
    def apply(start: Double2, dir: Double2): Ray2 = Ray2Over[Double](start, dir)
  }
  type Ray = RayOver[Double]
  object Ray{
    def apply(start: Double3, dir: Double3): Ray = RayOver[Double](start, dir)
  }
  type Rectangle2 = Rectangle2Over[Double]
  object Rectangle2{
    def apply(center: Double2, extent: Double2): Rectangle2 = Rectangle2Over[Double](center, extent)
  }
  type Rectangle = RectangleOver[Double]
  object Rectangle{
    def apply(center: Double3, right : Double3, up : Double3): Rectangle = RectangleOver[Double](center, right, up)
  }
  type Sphere = SphereOver[Double]
  object Sphere{
    def apply(center: Double3, rad: Double): Sphere = SphereOver[Double](center, rad)
  }
  type Square2 = Square2Over[Double]
  object Square2{
    def apply(center: Double2, extent: Double): Square2 = Square2Over(center, extent)
  }
  type Triangle = TriangleOver[Double]
  object Triangle{
    def apply(p1: Double3, p2: Double3, p3: Double3): TriangleOver[Double] = TriangleOver(p1, p2, p3)
  }
  type OBB2 = OBB2Over[Double]
  object OBB2{
    def apply(center: Double2, right: Double2, up: Double2, extentRight: Double, extentUp: Double): OBB2Over[Double] = OBB2Over(center, right, up, extentRight, extentUp)
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
  type AABBF = AABBOver[Float]
  object AABBF{
    def apply(center: Row[Float,_3], extent: Row[Float,_3]) = AABBOver[Float](center, extent)
  }
  type CircleF = CircleOver[Float]
  object CircleF{
    def apply(center : Row[Float,_2], rad: Float) = CircleOver(center, rad)
  }
  type Line2F = Line2Over[Float]
  object Line2F{
    def apply(start : Float2, end : Float2) = Line2Over[Float](start, end)
  }
  type LineF = LineOver[Float]
  object LineF{
    def apply(start: Float3, end: Float3) = LineOver[Float](start, end)
  }
  type OBBF = OBBOver[Float]
  object OBBF{
    def apply(center: Float3, right: Float3, up: Float3, extentRight: Float, extentUp: Float, extentLook: Float) = OBBOver(center, right, up, extentRight, extentUp, extentLook)
  }
  type PlaneF = PlaneOver[Float]
  object PlaneF{
    def apply(point: Float3, normal: Float3) = PlaneOver[Float](point, normal)
  }
  type Ray2F = Ray2Over[Float]
  object Ray2F{
    def apply(start: Float2, end: Float2) = Ray2Over[Float](start, end)
  }
  type RayF = RayOver[Float]
  object RayF{
    def apply(start: Float3, end: Float3) = RayOver[Float](start, end)
  }
  type Rectangle2F = Rectangle2Over[Float]
  object Rectangle2F{
    def apply(center: Float2, extent: Float2) = Rectangle2Over[Float](center, extent)
  }
  type RectangleF = RectangleOver[Float]
  object RectangleF{
    def apply(center: Float3, right : Float3, up : Float3) = RectangleOver[Float](center, right, up)
  }
  type SphereF = SphereOver[Float]
  object SphereF{
    def apply(center: Float3, rad: Float) = SphereOver[Float](center, rad)
  }
  type Square2F = Square2Over[Float]
  object Square2F{
    def apply(center: Float2, extent: Float) = Square2Over[Float](center, extent)
  }
  type TriangleF = TriangleOver[Float]
  object TriangleF{
    def apply(p1: Float3, p2: Float3, p3: Float3) = TriangleOver[Float](p1, p2, p3)
  }
  type OBB2F = OBB2Over[Float]
  object OBB2F{
    def apply(center: Float2, right: Float2, up: Float2, extentRight: Float, extentUp: Float): OBB2F = OBB2Over(center, right, up, extentRight, extentUp)
  }
  type Triangle2F = Triangle2Over[Float]
  object Triangle2F{
    def apply(p1: Float2, p2: Float2, p3: Float2) = Triangle2Over[Float](p1,p2,p3)
  }
  //--------------------------------------------------------


  //SOME SYNTACTIC GOODIES
  //used as fully infered function
  @inline def transformd(a: Double3, b: Mat4D) : Double3 = {
    val temp = Double4(a, 1D)
    val temp2 = temp ⨯ b
    Double3(temp2(0), temp2(1), temp(2))
  }
  @inline def transformf(a: Float3, b: Mat4F) : Float3 = {
    val temp = Float4(a, 1F)
    val temp2 = temp ⨯ b
    Float3(temp2(0), temp2(1), temp(2))
  }

  //........................
  

  


  object Double2{
    @inline def apply(x: Double, y: Double) = Row[Double, _2](x,y)
  }

  object Double3{
    @inline def apply(x: Double, y: Double, z: Double) = Row[Double, _3](x,y,z)
    @inline def apply(v2:Double2, z:Double) = Row[Double, _3](v2(0), v2(1), z)
  }

  object Double4{
    @inline def apply(x: Double, y: Double, z: Double, w: Double)  = Row[Double, _4](x,y,z,w)
    @inline def apply(v:Double3, w:Double): Double4 = Row[Double, _4](v(0), v(1), v(2), w)
  }


  object Float2{
    @inline def apply(x: Float, y: Float) = Row[Float, _2](x,y)
  }

  object Float3{
    @inline def apply(x: Float, y: Float, z: Float) = Row[Float, _3](x,y,z)
    @inline def apply(v2:Float2, z:Float) = Row[Float, _3](v2(0), v2(1), z)
  }

  object Float4{
    @inline def apply(x: Float, y: Float, z: Float, w: Float)  = Row[Float, _4](x,y,z,w)
    @inline def apply(v:Float3, w:Float): Float4 = Row[Float, _4](v(0), v(1), v(2), w)
  }
  


  object Int2{
    @inline def apply(x: Int, y: Int) = Row[Int, _2](x,y)
  }

  object Int3{
    @inline def apply(x: Int, y: Int, z: Int) = Row[Int, _3](x,y,z)
    @inline def apply(v2:Int2, z:Int) = Row[Int, _3](v2(0), v2(1), z)
  }

  object Int4{
    @inline def apply(x: Int, y: Int, z: Int, w: Int)  = Row[Int, _4](x,y,z,w)
    @inline def apply(v:Int3, w:Int): Int4 = Row[Int, _4](v(0), v(1), v(2), w)
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



      Mat[Float, _4](
        1F, 0F, 0F, 0F,
        0F, 1F, 0F, 0F,
        0F, 0F, 1F, 0F,
        0F, 0F, 0F, 1F)
    }

    def scale(x: Float, y: Float, z: Float): Mat4F =
    {
      Mat[Float, _4](
        x, 0F, 0F, 0F,
        0F, y, 0F, 0F,
        0F, 0F, z, 0F,
        0F, 0F, 0F, 1F)
    }

    def scale(v: Float3): Mat4F =
    {
      Mat[Float,_4](
        v(0), 0, 0, 0,
        0, v(1), 0, 0,
        0, 0, v(2), 0,
        0, 0, 0, 1)
    }

    def translation(x: Float, y: Float, z: Float): Mat4F =
    {
      Mat[Float,_4](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        x, y, z, 1)
    }

    def translation(v: Float3): Mat4F =
    {
      Mat[Float,_4](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        v(0), v(1), v(2), 1)
    }

    def rotationDeg(axis: Float3, angleInDegrees: Float): Mat4F =
    {
      val rad = angleInDegrees * scala.math.Pi / 180
      val cos = scala.math.cos(rad).toFloat
      val sin = scala.math.sin(rad).toFloat
      val x = axis(0)
      val y = axis(1)
      val z = axis(2)
      Mat[Float,_4](
        cos + x * x * (1 - cos), x * y * (1 - cos) - z * sin, x * z * (1 - cos) + y * sin, 0,
        y * x * (1 - cos) + z * sin, cos + y * y * (1 - cos), y * z * (1 - cos) - x * sin, 0,
        z * x * (1 - cos) - y * sin, z * y * (1 - cos) + x * sin, cos + z * z * (1 - cos), 0,
        0, 0, 0, 1)

    }

    def rotationRad(axis: Float3, angleInRadians: Float): Mat4F =
    {

      val cos = Math.cos(angleInRadians).toFloat
      val sin = Math.sin(angleInRadians).toFloat
      val x = axis(0)
      val y = axis(1)
      val z = axis(2)
      Mat[Float,_4](
        cos + x * x * (1F - cos), x * y * (1F - cos) - z * sin, x * z * (1F - cos) + y * sin, 0F,
        y * x * (1F - cos) + z * sin, cos + y * y * (1F - cos), y * z * (1F - cos) - x * sin, 0F,
        z * x * (1F - cos) - y * sin, z * y * (1F - cos) + x * sin, cos + z * z * (1F - cos), 0F,
        0F, 0F, 0F, 1F)

    }

    def ortho(left: Float, right: Float, bottom: Float, top: Float, near: Float, far: Float): Mat4F =
    {

      Mat[Float, _4](
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

      Mat[Float,_4](
        2 * near / (right - left), 0, (right + left) / (right - left), 0, //OpenGL form(column-major) not transposed, transposed - row-major form
        0, 2 * near / (top - bottom), (top + bottom) / (top - bottom), 0,
        0, 0, -(far + near) / (far - near), -2 * (far * near) / (far - near),
        0, 0, -1, 0)
    }




    def view(pos: Float3, target: Float3, up: Float3): Mat4F =
    {

      val za = (target - pos).normalize
      val xa = up.⨯(za).normalize
      val ya = za.⨯(xa)
      Mat[Float,_4](
        xa(0), ya(0), za(0), 0F,
        xa(1), ya(1), za(1), 0F,
        xa(2), ya(2), za(2), 0F,
        -xa.⋅(pos), -ya.⋅(pos), -za.⋅(pos), 1F)
    }

    def viewDir(pos: Vec3[Float], look: Vec3[Float], up: Vec3[Float]): Mat4F =
    {


      val za = -look
      val xa = up.⨯(za).normalize
      val ya = za.⨯(xa)
      Mat[Float,_4](
        xa(0), ya(0), za(0), 0F,
        xa(1), ya(1), za(1), 0F,
        xa(2), ya(2), za(2), 0F,
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


    //common transformations for V₃ over Double------------------------------------------
    def identity(): Mat4D = //row major
    {

      Mat[Double,_4](
        1D, 0D, 0D, 0D,
        0D, 1D, 0D, 0D,
        0D, 0D, 1D, 0D,
        0D, 0D, 0D, 1D)
    }

    def scale(x: Double, y: Double, z: Double): Mat4D = //row major
    {
      Mat[Double,_4](
        x, 0D, 0D, 0D,
        0D, y, 0D, 0D,
        0D, 0D, z, 0D,
        0D, 0D, 0D, 1D)
    }

    def scale(v: Vec3[Double]): Mat4D = //row major
    {
      Mat[Double,_4](
        v(0), 0, 0, 0,
        0, v(1), 0, 0,
        0, 0, v(2), 0,
        0, 0, 0, 1)
    }

    def translation(x: Double, y: Double, z: Double): Mat4D = //row major
    {
      Mat[Double,_4](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        x, y, z, 1)
    }

    def translation(v: Vec3[Double]): Mat4D = //row major
    {
      Mat[Double,_4](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        v(0), v(1), v(2), 1)
    }

    def rotationDeg(axis: Vec3[Double], angleInDegrees: Double): Mat4D = //row major
    {
      val rad = angleInDegrees * scala.math.Pi / 180
      val cos = scala.math.cos(rad)
      val sin = scala.math.sin(rad)
      val x = axis(0)
      val y = axis(1)
      val z = axis(2)
      Mat[Double,_4](
        cos + x * x * (1 - cos), x * y * (1 - cos) - z * sin, x * z * (1 - cos) + y * sin, 0,
        y * x * (1 - cos) + z * sin, cos + y * y * (1 - cos), y * z * (1 - cos) - x * sin, 0,
        z * x * (1 - cos) - y * sin, z * y * (1 - cos) + x * sin, cos + z * z * (1 - cos), 0,
        0, 0, 0, 1)

    }

    def rotationRad(axis: Vec3[Double], angleInRadians: Double): Mat4D = //row major
    {

      val cos = scala.math.cos(angleInRadians)
      val sin = scala.math.sin(angleInRadians)
      val x = axis(0)
      val y = axis(1)
      val z = axis(2)
      Mat[Double,_4](
        cos + x * x * (1D - cos), x * y * (1D - cos) - z * sin, x * z * (1D - cos) + y * sin, 0D,
        y * x * (1D - cos) + z * sin, cos + y * y * (1D - cos), y * z * (1D - cos) - x * sin, 0D,
        z * x * (1D - cos) - y * sin, z * y * (1D - cos) + x * sin, cos + z * z * (1D - cos), 0D,
        0D, 0D, 0D, 1D)

    }

    def ortho(left: Double, right: Double, bottom: Double, top: Double, near: Double, far: Double): Mat4D = //column major
    {

      Mat[Double,_4](
        2 / (right - left), 0, 0, -(right + left) / (right - left),
        0, 2 / (top - bottom), 0, -(top + bottom) / (top - bottom),
        0, 0, -2 / (far - near), -(far + near) / (far - near),
        0, 0, 0, 1)
    }


    def perspective(angleInDegrees: Double, aspect: Double, near: Double, far: Double): Mat4D = //column major
    {
      val top:Double = near * scala.math.tan(scala.math.Pi / 180 * angleInDegrees / 2)
      val bottom = -top
      val right = top * aspect
      val left = -right

      Mat[Double,_4](
        2 * near / (right - left), 0D, (right + left) / (right - left), 0D,
        0D, 2 * near / (top - bottom), (top + bottom) / (top - bottom), 0D,
        0, 0, -(far + near) / (far - near), -2 * (far * near) / (far - near),
        0, 0, -1, 0)
    }



    def view(pos: Vec3[Double], target: Vec3[Double], up: Vec3[Double]): Mat4D = //row major?
    {

      val za = (target - pos).normalize
      val xa = up.⨯(za).normalize
      val ya = za.⨯(xa)
      Mat[Double,_4](
        xa(0), ya(0), za(0), 0D,
        xa(1), ya(1), za(1), 0D,
        xa(2), ya(2), za(2), 0D,
        -xa.⋅(pos), -ya.⋅(pos), -za.⋅(pos), 1D)
    }

    def viewDir(pos: Vec3[Double], look: Vec3[Double], up: Vec3[Double]): Mat4D = //row major?
    {


      val za = -look
      val xa = up.⨯(za).normalize
      val ya = za.⨯(xa)
      Mat[Double,_4](
        xa(0), ya(0), za(0), 0D,
        xa(1), ya(1), za(1), 0D,
        xa(2), ya(2), za(2), 0D,
        -xa.⋅(pos), -ya.⋅(pos), -za.⋅(pos), 1D)
    }

    def view(pos: Vec3[Double], rotX: Double, rotY: Double, rotZ: Double): Mat4D = //row major?
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

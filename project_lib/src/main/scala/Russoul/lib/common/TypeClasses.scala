package Russoul.lib.common

import Russoul.lib.common.math.algebra._
import Russoul.lib.common.utils.Arr

import scala.language.{higherKinds, implicitConversions}
import scala.math.Ordering
import scala.reflect.ClassTag
import Implicits._
import Russoul.lib.common.Ops.FieldOps

/**
  * Created by russoul on 18.05.17.
  */
object TypeClasses {

  class OperationUnsupportedException(str: String) extends Exception(str)

  trait Container1[@specialized T, Con]{
    def x(con: Con): T
  }
  trait Container2[@specialized T, Con] extends Container1[T,Con]{
    def y(con: Con): T
  }
  trait Container3[@specialized T, Con] extends Container2[T,Con]{
    def z(con: Con): T
  }
  trait Container4[@specialized T, Con] extends Container3[T,Con]{
    def w(con: Con): T
  }

  trait ContainerAny[@specialized T, Con] extends Container4[T,Con]{
    def apply(con: Con, i: Int) : T
    def size(con: Con) : Int
  }


  trait Addable[@specialized A] {
    @inline def plus(x: A, y:A): A

    override def toString: String
  }

  //under mult
  trait Monoid[@specialized A]{
    def times(x : A, y: A) : A
    def one : A
  }

  trait CanBeNegated[@specialized A]{
    @inline def negate(x: A): A
  }

  trait Orderable[@specialized A] extends CanBeNegated[A] with Ordering[A]{
    @inline def abs(x:A): A = max(x, negate(x))
  }


  trait CommutativeGroup[@specialized A] extends Addable[A] with CanBeNegated[A]{
    @inline def zero: A
    @inline def minus(x: A, y: A): A = plus(x, negate(y))
  }

  trait Trig[@specialized A]{
    @inline def atan2(x:A, y:A) : A
    @inline def toRadians(x:A): A
    @inline def cos(x:A): A
    @inline def sin(x:A): A
    @inline def acos(x:A):A
    def atan(x: A) : A
  }

  trait Euclidean[@specialized A]
  {
    @inline def sqrt(x:A) : A
    @inline def pow(x: A, y:A): A
    def cbrt(x: A) : A
  }

  //means that its is dim is 1
  trait ConvertibleFromDouble[@specialized A]{
    @inline def fromDouble(x: Double) : A
  }

  trait Ring[@specialized A] extends CommutativeGroup[A] with Monoid[A]

  trait Field[@specialized A] extends Ring[A]{
    @inline def inv(x: A): A
    @inline def div(x: A, y: A): A = times(x, inv(y))

  }


  trait ModuleOverRing[V, @specialized R] extends CommutativeGroup[V]{
    implicit def scalar: Ring[R]

    def dimensions : Int

    //@inline def plus(a:V, b:V) : V //can't use the same names because of type erasure
    //@inline def negate(a:V):V
    @inline def times(a:V, k:R):V
    //@inline def zero(dim:Int) : V TODO

    //@inline def minus(a:V, b:V):V = plus(a, negate(b))

    @inline def create(coordinates: R*) : V

    @inline def get(v:V, i:Int) : R

    @inline @straight def x(v: V) : R
    @inline @straight def y(v: V) : R = throw new OperationUnsupportedException("")
    @inline @straight def z(v: V) : R = throw new OperationUnsupportedException("")
    @inline @straight def w(v: V) : R = throw new OperationUnsupportedException("")

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */

    //TODO Modules in general do not support this operation !
    @inline def timesByElement(a: V, b: V) : V
  }



  trait VectorSpaceOverField[V,@specialized F] extends ModuleOverRing[V,F]{

    override implicit def scalar: Field[F]
    @inline def div(a:V, k:F):V = times(a, scalar.inv(k))

  }


 /* trait ElementOps2[@specialized T]{
    @inline def x: T
    @inline def y: T
  }

  trait ElementOps3[@specialized T]{
    @inline def x: T
    @inline def y: T
    @inline def z: T
  }

  trait ElementOps4[@specialized T]{
    @inline def x: T
    @inline def y: T
    @inline def z: T
    @inline def w: T
  }*/


  //used to better utilise implicits (being more concrete with what the matrix should be)
  trait MixinForMutables[A, B]{
    def mixin(obj: A) : A with B
    def mixinCopy(obj: A) : A with B
  }



  trait Gram
  trait CrossProduct

  //used to specify the area where the matrix may be used (great for implicits)
  class GramMixin[@specialized F : ClassTag](implicit ev: Field[F]) extends MixinForMutables[Mat[F], Gram]{
    /**
      *
      * @param mat
      * @return user shall not change contents of input "mat" as they point to the same Arr as the result
      *         faster than copying
      */
    def mixin(mat:  Mat[F] @mutable): Mat[F] with Gram =
    {
      new Mat[F](mat.rows, mat.columns, mat.ar) with Gram
    }

    /**
      *
      * @param mat
      * @return copy version of mixin, slower but the result is absolutely independent object
      */
    def mixinCopy(mat:  Mat[F] @mutable): Mat[F] with Gram =
    {
      new Mat[F](mat.rows, mat.columns, mat.ar.clone()) with Gram
    }

  }

  //used to specify the area where the matrix may be used (great for implicits)
  class CrossProductMixin[@specialized F : ClassTag](implicit ev: Field[F]) extends MixinForMutables[Mat[F], CrossProduct]{
    /**
      *
      * @param mat
      * @return user shall not change contents of input "mat" as they point to the same Arr as the result
      *         faster than copying
      */
    def mixin(mat:  Mat[F] @mutable): Mat[F] with CrossProduct =
    {
      new Mat[F](mat.rows, mat.columns, mat.ar) with CrossProduct
    }

    /**
      *
      * @param mat
      * @return copy version of mixin, slower but the result is absolutely independent object
      */
    def mixinCopy(mat:  Mat[F] @mutable): Mat[F] with CrossProduct =
    {
      new Mat[F](mat.rows, mat.columns, mat.ar.clone()) with CrossProduct
    }

  }


  trait GramCrossProductOp[V]{
    /**
      *
      * @param a
      * @param b
      * @param ijk [i,i] [i,j] [i,k]
      *            [j,i] [j,j] [j,k]
      *            [k,i] [k,j] [k,k]
      *            only for dim 3
      * @return
      */
    def crossProduct(a: V, b: V, ijk : Mat[V] with CrossProduct) : V
  }

  trait CanonicalCrossProductOp[V]{
    def crossProduct(a: V, b: V) : V
  }

  trait Mat4Mult[V, @specialized F]{
    def multM(v : V, mat : Mat4[F]) : V
  }

  trait Canonical2DimOrthoOp[V]{
    def ortho(a: V) : V
  }

  trait EuclideanSpaceOverField[V,@specialized F] extends VectorSpaceOverField[V,F]{

    override implicit def scalar : Field[F] with Trig[F] with Euclidean[F]

    @inline def dotProduct(a:V, b:V, matGram:Mat[F] with Gram):F

    @inline def squaredLength(a:V, matGram:Mat[F] with Gram):F = dotProduct(a,a, matGram)
    @inline def length(a:V, matGram:Mat[F] with Gram):F = scalar.sqrt(dotProduct(a,a, matGram))
    @inline def angle(a:V, b:V, matGram:Mat[F] with Gram):F = scalar.acos(scalar.div(scalar.div(dotProduct(a, b, matGram), length(a, matGram)), length(b, matGram)))

    @inline def canonicalBasis(dim:Int) : Arr[V]

    @inline def normalize(v: V, matGram : Mat[F] with Gram) : V = {
      val len = length(v, matGram)

      div(v, len)
    }


  }


  //using canonical basis
  trait CanonicalEuclideanSpaceOverField[V,@specialized F] extends VectorSpaceOverField[V,F] {

    override implicit def scalar: Field[F] with Trig[F] with Euclidean[F]

    @inline def dotProduct(a: V, b: V): F

    @inline def squaredLength(a: V): F = dotProduct(a, a)

    @inline def length(a: V): F = scalar.sqrt(dotProduct(a, a))

    @inline def angle(a: V, b: V): F = scalar.acos(scalar.div(scalar.div(dotProduct(a, b), length(a)), length(b)))

    @inline def normalize(v: V): V = {
      val len = length(v)
      div(v, len)
    }

  }

  //DEFINITIONS------------------------------------------------------


  trait IntIsAddable extends Addable[Int]{
    override def plus(x: Int, y: Int): Int = x + y
  }

  trait IntIsCommutativeGroup extends CommutativeGroup[Int]{
    override def negate(x: Int): Int = -x
    override def zero: Int = 0
    override def plus(x: Int, y: Int): Int = x + y

    override def toString: String = {
      "Int"
    }


  }


  trait IntIsOrderable extends Orderable[Int] with Ordering.IntOrdering{
    override def negate(x: Int): Int = -x
  }

  trait IntIsRing extends IntIsCommutativeGroup with Ring[Int]{
    override def times(x: Int, y: Int): Int = x * y
    override def one: Int = 1
  }

  class IntIsFullRing extends IntIsRing with IntIsOrderable


  trait FloatIsTrig extends Trig[Float]{
    override def atan2(x: Float, y:Float): Float = Math.atan2(x,y).toFloat


    override def atan(x: RealF): RealF = Math.atan(x).toFloat

    override def cos(x: Float): Float = Math.cos(x).toFloat
    override def sin(x: Float): Float = Math.sin(x).toFloat


    override def acos(x: Float): Float = Math.acos(x).toFloat

    override def toRadians(x: Float): Float = {
      Math.toRadians(x).toFloat
    }


  }


  trait FloatIsConvertibleFromDouble extends ConvertibleFromDouble[Float]{
    override def fromDouble(x: Double): Float = x.toFloat


  }


  trait FloatIsEuclidean extends Euclidean[Float]{
    override def sqrt(x: Float): Float = Math.sqrt(x).toFloat
    override def pow(x: Float, y:Float): Float = Math.pow(x,y).toFloat

    override def cbrt(x: RealF): RealF = Math.cbrt(x).toFloat
  }


  trait FloatIsField extends Field[Float]{
    override def times(x: Float, y: Float): Float = x * y
    override def inv(x: Float): Float = 1/x
    override def one: Float = 1F
    override def negate(x: Float): Float = -x
    override def zero: Float = 0F
    override def plus(x: Float, y: Float): Float = x + y
    override def toString: String = {
      "Float"
    }

  }

  trait FloatIsOrderable extends Orderable[Float] with Ordering.FloatOrdering{
    override def negate(x: Float): Float = -x
  }

  trait DoubleIsConvertibleFromDouble extends ConvertibleFromDouble[Double]{
    override def fromDouble(x: Double): Double = x

  }



  trait DoubleIsTrig extends Trig[Double]{
    override def atan2(x: Double, y:Double): Double = Math.atan2(x,y)

    override def cos(x: Double): Double = Math.cos(x)
    override def sin(x: Double): Double = Math.sin(x)


    override def acos(x: Real): Real = Math.acos(x)


    override def atan(x: Real): Real = Math.atan(x)

    override def toRadians(x: Double): Double = {
      Math.toRadians(x)
    }

  }
  trait DoubleIsOrderable extends Orderable[Double] with Ordering.DoubleOrdering{
    override def negate(x: Real): Real = -x
  }


  trait DoubleIsEuclidean extends Euclidean[Double]{
    override def sqrt(x: Double): Double = Math.sqrt(x)
    override def pow(x: Double, y:Double): Double = Math.pow(x,y)

    override def cbrt(x: Real): Real = Math.cbrt(x)
  }

  trait DoubleIsField extends Field[Double]{
    override def times(x: Double, y: Double): Double = x * y
    override def inv(x: Double): Double = 1/x
    override def one: Double = 1D
    override def negate(x: Double): Double = -x
    override def zero: Double = 0D
    override def plus(x: Double, y: Double): Double = x + y

    override def toString: String = {
      "Double"
    }


  }


  class DoubleIsFullField extends DoubleIsField with DoubleIsTrig with DoubleIsEuclidean with DoubleIsConvertibleFromDouble with DoubleIsOrderable
  object DoubleIsFullField{
    implicit object Implicit extends DoubleIsFullField
  }

  class FloatIsFullField extends FloatIsField with FloatIsTrig with FloatIsEuclidean with FloatIsConvertibleFromDouble with FloatIsOrderable



  class ComplexIsField extends Field[ComplexOver[Real]]{

    import DoubleIsFullField._

    override def times(x: ComplexOver[Real], y: ComplexOver[Real]): ComplexOver[Real] = x * y
    override def inv(x: ComplexOver[Real]): ComplexOver[Real] = one / x
    override def one: ComplexOver[Real] = ComplexOver(1D,0D)
    override def negate(x: ComplexOver[Real]): ComplexOver[Real] = -x
    override def zero: ComplexOver[Real] = ComplexOver(0D,0D)
    override def plus(x: ComplexOver[Real], y: ComplexOver[Real]): ComplexOver[Real] = x + y

  }

  class Double3IsField extends Field[(Double, Double, Double)]{
    override def times(x: (Double, Double, Double), y: (Double, Double, Double)): (Double, Double, Double) = (x._1 * y._1, x._2 * y._2, x._3 * y._3)
    override def inv(x: (Double, Double, Double)): (Double, Double, Double) = (1/x._1, 1/x._2, 1/x._3)
    override def one: (Double, Double, Double) = (1D,1D,1D)
    override def negate(x: (Double, Double, Double)): (Double, Double, Double) = (-x._1, -x._2, -x._3)
    override def zero: (Double, Double, Double) = (0D,0D,0D)
    override def plus(x: (Double, Double, Double), y: (Double, Double, Double)): (Double, Double, Double) = (x._1 + y._1, x._2 + y._2, x._3 + y._3)

    override def toString: String = {
      "Double3"
    }
  }



  class Int2IsModule2OverInt extends ModuleOverRing[Int2, Int]{


    override def dimensions: Int = 2

    override implicit val scalar: Ring[Int] = new IntIsFullRing

    override def plus(a: Int2, b: Int2): Int2 = Int2(a.x + b.x, a.y + b.y)

    override def negate(a: Int2): Int2 = Int2(-a.x, -a.y)

    override def times(a: Int2, k: Int): Int2 = Int2(a.x * k, a.y * k)

    override def zero: Int2 = Int2(0,0)

    override def create(coordinates: Int*): Int2 = Int2(coordinates(0), coordinates(1))

    override def get(v: Int2, i: Int): Int = v(i-1)

    override def x(v: Int2): Int = v.x

    override def y(v: Int2): Int = v.y


    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Int2, b: Int2): Int2 = Int2(a.x * b.x, a.y * b.y)
  }


  class Int3IsModule3OverInt extends ModuleOverRing[Int3, Int]{


    override def dimensions: Int = 3

    override implicit val scalar: Ring[Int] = new IntIsFullRing

    override def plus(a: Int3, b: Int3): Int3 = Int3(a.x + b.x, a.y + b.y, a.z + b.z)

    override def negate(a: Int3): Int3 = Int3(-a.x, -a.y, -a.z)

    override def times(a: Int3, k: Int): Int3 = Int3(a.x * k, a.y * k, a.z * k)

    override def zero: Int3 = Int3(0,0,0)

    override def create(coordinates: Int*): Int3 = Int3(coordinates(0), coordinates(1), coordinates(2))

    override def get(v: Int3, i: Int): Int = v(i-1)

    override def x(v: Int3): Int = v.x

    override def y(v: Int3): Int = v.y

    override def z(v: Int3): Int = v.z

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Int3, b: Int3): Int3 = Int3(a.x * b.x, a.y * b.y, a.z * b.z)
  }



  class TurpleDouble3IsEuclideanSpace3OverDouble extends EuclideanSpaceOverField[(Double,Double,Double), Double] with GramCrossProductOp[(Double,Double,Double)]{


    override def dimensions: Int = 3

    override implicit val scalar: Field[Real] with Trig[Real] with Euclidean[Real] = new DoubleIsFullField

    override def dotProduct(a: (Double, Double, Double), b: (Double, Double, Double), gram: Mat[Double] with Gram): Double = {
      //TODO dim check ??? must be 3 !


      a._1 * b._1 * gram(0,0) + a._1 * b._2 * gram(0,1) + a._1 * b._3 * gram(0,2) +
        a._2 * b._1 * gram(1,0) + a._2 * b._2 * gram(1,1) + a._2 * b._3 * gram(1,2) +
        a._3 * b._1 * gram(2,0) + a._3 * b._2 * gram(2,1) + a._3 * b._3 * gram(2,2)

    }


    /**
      *
      * @param a
      * @param b
      * @param ijk [i,i] [i,j] [i,k]
      *            [j,i] [j,j] [j,k]
      *            [k,i] [k,j] [k,k]
      * @return
      */
    override def crossProduct(a: (Real, Real, Real), b: (Real, Real, Real), ijk: Mat[(Double, Double, Double)] with CrossProduct): (Real, Real, Real) = {
      val i0 = times(ijk(0,0), a._1 * b._1)
      val i1 = times(ijk(0,1), a._1 * b._2)
      val i2 = times(ijk(0,2), a._1 * b._3)
      val i3 = times(ijk(1,0), a._2 * b._1)
      val i4 = times(ijk(1,1), a._2 * b._2)
      val i5 = times(ijk(1,2), a._2 * b._3)
      val i6 = times(ijk(2,0), a._3 * b._1)
      val i7 = times(ijk(2,1), a._3 * b._2)
      val i8 = times(ijk(2,2), a._3 * b._3)

      (i0._1 + i1._1+ i2._1 + i3._1 + i4._1 + i5._1 + i6._1+ i7._1 + i8._1,
        i0._2 + i1._2+ i2._2 + i3._2 + i4._2 + i5._2 + i6._2+ i7._2 + i8._2,
        i0._3 + i1._3+ i2._3 + i3._3 + i4._3 + i5._3 + i6._3+ i7._3 + i8._3
      )
    }

    override def plus(a: (Double, Double, Double), b: (Double, Double, Double)): (Double, Double, Double) = {
      (a._1 + b._1, a._2 + b._2, a._3 + b._3)
    }

    override def negate(a: (Double, Double, Double)): (Double, Double, Double) = {
      (-a._1, -a._2, -a._3)
    }

    override def times(a: (Double, Double, Double), k: Double): (Double, Double, Double) = {
      (a._1 * k, a._2 * k, a._3 * k)
    }

    override def zero: (Double, Double, Double) = {
      (0,0,0)
    }


    //TODO inefficient
    override def get(v: (Real, Real, Real), i: Int): Real = {
      i match{
        case 1 => v._1
        case 2 => v._2
        case 3 => v._3
        case _ => throw new Exception("Incorrect index !")
      }
    }





    override def x(v: (Real, Real, Real)): Real = v._1

    override def y(v: (Real, Real, Real)): Real = v._2

    override def z(v: (Real, Real, Real)): Real = v._3



    override def create(coordinates: Double*): (Double, Double, Double) = {
      (coordinates(0), coordinates(1), coordinates(2))
    }


    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: (Real, Real, Real), b: (Real, Real, Real)): (Real, Real, Real) = {
      (a._1 * b._1, a._2 * b._2, a._3 * b._3)
    }

    def canonicalBasis(dim:Int):Arr[(Double, Double, Double)] = Arr((1D,0,0), (0,1D,0), (0,0,1D))
  }


  class VecIsCanonicalEuclideanSpaceOverReal(final val dimensions: Int) extends CanonicalEuclideanSpaceOverField[Vec[Real], Real]{



    override implicit val scalar: Field[Real] with Trig[Real] with Euclidean[Real] = new DoubleIsFullField

    /**
      *
      * @param a will be set as row
      * @param b will be set as column
      * @return
      */
    override def dotProduct(a: Vec[Real], b: Vec[Real]): Real = {

      var x = 0 : Real
      var k = 1
      while(k <= a.size()){
        x += a(k) * b(k)
        k += 1
      }

      x
    }

    override def plus(a: Vec[Real], b: Vec[Real]): Vec[Real] = a + b

    override def negate(a: Vec[Real]): Vec[Real] = -a

    override def times(a: Vec[Real], k: Real): Vec[Real] = a * k

    override def zero: Vec[Real] = new Vec[Real](new Array[Real](0)) //TODO

    override def create(coordinates: Real*): Vec[Real] = {
      val ar = new Array[Real](coordinates.size)
      var i = 0
      while(i < ar.size){
        ar(i) = coordinates(i)
        i += 1
      }

      Vec[Real](ar)
    }

    override def get(v: Vec[Real], i: Int): Real = v(i)

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Vec[Real], b: Vec[Real]): Vec[Real] = {
      val ar = new Array[Real](a.size())
      var k = 0
      while(k < ar.length){
        ar(k) = a(k) * b(k)
        k += 1
      }

      Vec[Real](ar)
    }

    override def x(v: Vec[Real]) = v(1)

    override def y(v: Vec[Real]) = v(2)

    override def z(v: Vec[Real]) = v(3)

    override def w(v: Vec[Real]) = v(4)
  }


  class Vec3IsCanonicalEuclideanSpaceOverField[@specialized T : ClassTag](field: Field[T] with Trig[T] with Euclidean[T]) extends CanonicalEuclideanSpaceOverField[Vec3[T], T] with CanonicalCrossProductOp[Vec3[T]] with Mat4Mult[Vec3[T],T]{


    override implicit val scalar: Field[T] with Trig[T] with Euclidean[T] = field

    override def dotProduct(a: Vec3[T], b: Vec3[T]): T = a.x * b.x + a.y * b.y + a.z * b.z

    override def crossProduct(a: Vec3[T], b: Vec3[T]): Vec3[T] = Vec3(a.y*b.x - b.y*a.z, -a.x*b.z + b.x*a.z, a.x * b.y - b.x * a.y)

    val V4 = new Vec4IsCanonicalEuclideanSpaceOverField[T](field)
    override def multM(v: Vec3[T], m: Mat4[T]): Vec3[T] = {
      val v4 = Vec4[T](x(v), y(v), z(v), scalar.zero)
      Vec3[T](V4.dotProduct(v4, m.column(1)), V4.dotProduct(v4, m.column(2)), V4.dotProduct(v4, m.column(3)))
    }

    override def dimensions: Int = 3

    override def times(a: Vec3[T], k: T): Vec3[T] = Vec3[T](a.x * k, a.y * k, a.z * k)

    override def create(coordinates: T*): Vec3[T] = Vec3(coordinates(0), coordinates(1), coordinates(2))

    override def get(v: Vec3[T], i: Int): T = v(i)

    override def x(v: Vec3[T]): T = v.x

    override def y(v: Vec3[T]) = v.y

    override def z(v: Vec3[T]) = v.z

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Vec3[T], b: Vec3[T]): Vec3[T] = Vec3[T](a.x * b.x, a.y * b.y, a.z * b.z)

    override def zero: Vec3[T] = Vec3(scalar.zero, scalar.zero, scalar.zero)

    override def plus(a: Vec3[T], b: Vec3[T]): Vec3[T] = Vec3[T](a.x + b.x, a.y + b.y, a.z + b.z)

    override def negate(a: Vec3[T]): Vec3[T] = Vec3[T](-a.x, -a.y, -a.z)
  }

  class Vec4IsCanonicalEuclideanSpaceOverField[@specialized T : ClassTag](field: Field[T] with Trig[T] with Euclidean[T]) extends CanonicalEuclideanSpaceOverField[Vec4[T], T] with Mat4Mult[Vec4[T],T]{


    override implicit val scalar: Field[T] with Trig[T] with Euclidean[T] = field

    override def dotProduct(a: Vec4[T], b: Vec4[T]): T = a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w


    override def multM(v: Vec4[T], m: Mat4[T]): Vec4[T] = {

      Vec4[T](dotProduct(v, m.column(1)), dotProduct(v, m.column(2)), dotProduct(v, m.column(3)), dotProduct(v, m.column(4)))
    }

    override def dimensions: Int = 4

    override def times(a: Vec4[T], k: T): Vec4[T] = Vec4[T](a.x * k, a.y * k, a.z * k, a.w * k)

    override def create(coordinates: T*): Vec4[T] = Vec4(coordinates(0), coordinates(1), coordinates(2), coordinates(3))

    override def get(v: Vec4[T], i: Int): T = v(i)

    override def x(v: Vec4[T]): T = v.x



    override def y(v: Vec4[T]) = v.y

    override def z(v: Vec4[T]) = v.z

    override def w(v: Vec4[T]) = v.w

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Vec4[T], b: Vec4[T]): Vec4[T] = Vec4[T](a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)

    override def zero: Vec4[T] = Vec4(scalar.zero, scalar.zero, scalar.zero, scalar.zero)

    override def plus(a: Vec4[T], b: Vec4[T]): Vec4[T] = Vec4[T](a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)

    override def negate(a: Vec4[T]): Vec4[T] = Vec4[T](-a.x, -a.y, -a.z, -a.w)
  }

  class Vec2IsCanonicalEuclideanSpaceOverField[@specialized T : ClassTag](field: Field[T] with Trig[T] with Euclidean[T]) extends CanonicalEuclideanSpaceOverField[Vec2[T], T] with Canonical2DimOrthoOp[Vec2[T]]{


    override implicit val scalar: Field[T] with Trig[T] with Euclidean[T] = field

    override def dotProduct(a: Vec2[T], b: Vec2[T]): T = a.x * b.x + a.y * b.y


    override def ortho(a: Vec2[T]): Vec2[T] = {
      Vec2[T](-a.y, a.x)
    }

    override def dimensions: Int = 2

    override def times(a: Vec2[T], k: T): Vec2[T] = Vec2[T](a.x * k, a.y * k)

    override def create(coordinates: T*): Vec2[T] = Vec2(coordinates(0), coordinates(1))

    override def get(v: Vec2[T], i: Int): T = v(i)

    override def x(v: Vec2[T]): T = v.x

    override def y(v: Vec2[T]) = v.y

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Vec2[T], b: Vec2[T]): Vec2[T] = Vec2[T](a.x * b.x, a.y * b.y)

    override def zero: Vec2[T] = Vec2(scalar.zero, scalar.zero)

    override def plus(a: Vec2[T], b: Vec2[T]): Vec2[T] = Vec2[T](a.x + b.x, a.y + b.y)

    override def negate(a: Vec2[T]): Vec2[T] = Vec2[T](-a.x, -a.y)
  }






  
  class Tuple2IsContainer2[@specialized T] extends Container2[T, (T,T)]{
    override def x(v: (T,T)): T = v._1

    override def y(v: (T,T)): T = v._2
  }
  class Tuple3IsContainer3[@specialized T] extends Container3[T, (T,T,T)]{
    override def x(v: (T,T,T)): T = v._1

    override def y(v: (T,T,T)): T = v._2

    override def z(v: (T,T,T)): T = v._3
  }
  class Tuple4IsContainer4[@specialized T] extends Container4[T, (T,T,T,T)]{
    override def x(v: (T,T,T,T)): T = v._1

    override def y(v: (T,T,T,T)): T = v._2

    override def z(v: (T,T,T,T)): T = v._3

    override def w(v: (T,T,T,T)): T = v._4
  }
  class Vec2IsContainer2[@specialized T] extends Container2[T, Vec2[T]]{
    override def x(v: Vec2[T]): T = v.x

    override def y(v: Vec2[T]): T = v.y
  }
  class Vec3IsContainer3[@specialized T] extends Container3[T, Vec3[T]]{
    override def x(v: Vec3[T]): T = v.x

    override def y(v: Vec3[T]): T = v.y

    override def z(v: Vec3[T]): T = v.z
  }
  class Vec4IsContainer4[@specialized T] extends Container4[T, Vec4[T]]{
    override def x(v: Vec4[T]): T = v.x

    override def y(v: Vec4[T]): T = v.y

    override def z(v: Vec4[T]): T = v.z

    override def w(v: Vec4[T]): T = v.w
  }
  class ArrayIsContainerAny[@specialized T] extends ContainerAny[T, Array[T]]{
    override def x(v: Array[T]): T = v(0)

    override def y(v: Array[T]): T = v(1)

    override def z(v: Array[T]): T = v(2)

    override def w(v: Array[T]): T = v(3)

    override def apply(con: Array[T], i: Int): T = con(i)

    override def size(con: Array[T]): Int = con.size
  }






  /*class Float3IsCanonicalEuclideanSpace3OverFloat extends CanonicalEuclideanSpaceOverField[Float3, Float] with CanonicalCrossProductOp[Float3] with Mat4Mult[Float3,Float]{


    override def dimensions: Int = 3

    override implicit val scalar: Field[RealF] with Trig[RealF] with Euclidean[RealF] = new FloatIsFullField

    override def dotProduct(a: Float3, b: Float3): Float = {
      //TODO dim check ??? must be 3 !
      a.x * b.x + a.y * b.y + a.z * b.z
    }

    val envV4 = implicitly[CanonicalEuclideanSpaceOverField[Float4, Float] with Mat4Mult[Float4, Float]]
    override def multM(v: Float3, mat: Mat4[RealF]): Float3 = {

      val v4 = envV4.create(x(v), y(v), z(v), envV4.scalar.zero)
      val v4Res = envV4.multM(v4, mat)

      create(envV4.x(v4Res), envV4.y(v4Res), envV4.z(v4Res))
    }

    /**
      *
      * @param a
      * @param b
      * @return
      */
    override def crossProduct(a: Float3, b: Float3): Float3 = {

      Float3(a.y*b.x - b.y*a.z, -a.x*b.z + b.x*a.z, a.x * b.y - b.x * a.y)
    }

    override def plus(a: Float3, b: Float3): Float3 = {
      Float3(a.x + b.x, a.y + b.y, a.z + b.z)
    }

    override def negate(a:Float3): Float3 = {
      Float3(-a.x, -a.y, -a.z)
    }

    override def times(a: Float3, k: Float): Float3 = {
      Float3(a.x * k, a.y * k, a.z * k)
    }

    override def zero: Float3 = {
      Float3(0F,0F,0F)
    }


    override def create(coordinates: Float*): Float3 = {
      Float3(coordinates(0), coordinates(1), coordinates(2))
    }


    override def get(v: Float3, i: Int): Float = v(i)


    override def x(v: Float3): Float = v.x

    override def y(v: Float3): Float = v.y

    override def z(v: Float3): Float = v.z

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Float3, b: Float3): Float3 = Float3(a.x * b.x, a.y * b.y, a.z * b.z)
  }

  class Double3IsCanonicalEuclideanSpace3OverDouble extends CanonicalEuclideanSpaceOverField[Real3, Real] with CanonicalCrossProductOp[Real3] with Mat4Mult[Real3, Real]{


    override def dimensions: Int = 3

    override implicit val scalar: Field[Real] with Trig[Real] with Euclidean[Real] = new DoubleIsFullField

    override def dotProduct(a: Real3, b: Real3): Real = {
      //TODO dim check ??? must be 3 !
      a.x * b.x + a.y * b.y + a.z * b.z
    }

    /**
      *
      * @param a
      * @param b
      * @return
      */
    override def crossProduct(a: Real3, b: Real3): Real3 = {

      Real3(a.y*b.x - b.y*a.z, -a.x*b.z + b.x*a.z, a.x * b.y - b.x * a.y)
    }


    val envV4 = implicitly[CanonicalEuclideanSpaceOverField[Real4, Real] with Mat4Mult[Real4, Real]]
    override def multM(v: Real3, mat: Mat4[Real]): Real3 = {

      val v4:Real4 = envV4.create(x(v), y(v), z(v), envV4.scalar.zero)
      val v4Res = envV4.multM(v4, mat)

      create(envV4.x(v4Res), envV4.y(v4Res), envV4.z(v4Res))
    }



    override def plus(a: Real3, b: Real3): Real3 = {
      Real3(a.x + b.x, a.y + b.y, a.z + b.z)
    }

    override def negate(a:Real3): Real3 = {
      Real3(-a.x, -a.y, -a.z)
    }

    override def times(a: Real3, k: Real): Real3 = {
      Real3(a.x * k, a.y * k, a.z * k)
    }

    override def zero: Real3 = {
      Real3(0D,0D,0D)
    }


    override def create(coordinates: Real*): Real3 = {
      Real3(coordinates(0), coordinates(1), coordinates(2))
    }


    override def get(v: Real3, i: Int): Real = v(i)


    override def x(v: Real3): Real = v.x

    override def y(v: Real3): Real = v.y

    override def z(v: Real3): Real = v.z

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Real3, b: Real3): Real3 = Real3(a.x * b.x, a.y * b.y, a.z * b.z)
  }

  class Double4IsCanonicalEuclideanSpace4OverDouble extends CanonicalEuclideanSpaceOverField[Real4, Real] with Mat4Mult[Real4, Real]{


    override def dimensions: Int = 4

    override implicit val scalar: Field[Real] with Trig[Real] with Euclidean[Real] = new DoubleIsFullField

    override def dotProduct(a: Real4, b: Real4): Real = {
      //TODO dim check ??? must be 3 !
      a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
    }


    override def plus(a: Real4, b: Real4): Real4 = {
      Real4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
    }

    override def negate(a:Real4): Real4 = {
      Real4(-a.x, -a.y, -a.z, -a.w)
    }

    override def times(a: Real4, k: Real): Real4 = {
      Real4(a.x * k, a.y * k, a.z * k, a.w * k)
    }

    override def zero: Real4 = {
      Real4(0D,0D,0D,0D)
    }


    override def create(coordinates: Real*): Real4 = {
      Real4(coordinates(0), coordinates(1), coordinates(2), coordinates(3))
    }


    override def multM(v: Real4, m: Mat4[Real]): Real4 = {

      Real4(dotProduct(v, m.column(1)), dotProduct(v, m.column(2)), dotProduct(v, m.column(3)), dotProduct(v, m.column(4)))
    }

    override def get(v: Real4, i: Int): Real = v(i)


    override def x(v: Real4): Real = v.x

    override def y(v: Real4): Real = v.y

    override def z(v: Real4): Real = v.z

    override def w(v: Real4): Real = v.w

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Real4, b: Real4): Real4 = Real4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
  }

  class Double2IsCanonicalEuclideanSpace2OverDouble extends CanonicalEuclideanSpaceOverField[Real2, Real] with Canonical2DimOrthoOp[Real2]{


    override def ortho(a: Real2): Real2 = {
      create(-y(a), x(a))
    }

    override def dimensions: Int = 2

    override implicit val scalar: Field[Real] with Trig[Real] with Euclidean[Real] = new DoubleIsFullField

    override def dotProduct(a: Real2, b: Real2): Real = {
      a(1) * b(1) + a(2) * b(2)

    }

    override def plus(a: Real2, b: Real2): Real2 = {
      Real2(a(1) + b(1), a(2) + b(2))
    }

    override def negate(a:Real2): Real2 = {
      Real2(-a(1), -a(2))
    }

    override def times(a: Real2, k: Real): Real2 = {
      Real2(a(1) * k, a(2) * k)
    }

    override def zero: Real2 = {
      Real2(0D,0D)
    }



    override def create(coordinates: Real*): Real2 = {
      Real2(coordinates(0), coordinates(1))
    }

    override def get(v: Real2, i: Int): Real = v(i)

    override def x(v: Real2): Real = v(1)

    override def y(v: Real2): Real = v(2)

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Real2, b: Real2): Real2 = Real2(a.x * b.x, a.y * b.y)
  }


  class Float4IsCanonicalEuclideanSpace4OverFloat extends CanonicalEuclideanSpaceOverField[Float4, Float] with Mat4Mult[Float4, Float]{


    override def dimensions: Int = 4

    override implicit val scalar: Field[RealF] with Trig[RealF] with Euclidean[RealF] = new FloatIsFullField

    override def dotProduct(a: Float4, b: Float4): Float = {
      //TODO dim check ??? must be 3 !
      a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
    }


    override def plus(a: Float4, b: Float4): Float4 = {
      Float4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
    }

    override def negate(a:Float4): Float4 = {
      Float4(-a.x, -a.y, -a.z, -a.w)
    }

    override def times(a: Float4, k: Float): Float4 = {
      Float4(a.x * k, a.y * k, a.z * k, a.w * k)
    }

    override def zero: Float4 = {
      Float4(0F,0F,0F,0F)
    }


    override def create(coordinates: Float*): Float4 = {
      Float4(coordinates(0), coordinates(1), coordinates(2), coordinates(3))
    }


    override def multM(v: Float4, m: Mat4[Float]): Float4 = {

      Float4(dotProduct(v, m.column(1)), dotProduct(v, m.column(2)), dotProduct(v, m.column(3)), dotProduct(v, m.column(4)))
    }

    override def get(v: Float4, i: Int): Float = v(i)


    override def x(v: Float4): Float = v.x

    override def y(v: Float4): Float = v.y

    override def z(v: Float4): Float = v.z

    override def w(v: Float4): Float = v.w

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Float4, b: Float4): Float4 = Float4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
  }



  class Float2IsCanonicalEuclideanSpace2OverFloat extends CanonicalEuclideanSpaceOverField[Float2, Float] with Canonical2DimOrthoOp[Float2]{


    override def ortho(a: Float2): Float2 = {
      create(-y(a), x(a))
    }

    override def dimensions: Int = 2

    override implicit val scalar: Field[RealF] with Trig[RealF] with Euclidean[RealF] = new FloatIsFullField


    override def dotProduct(a: Float2, b: Float2): Float = {
      a(1) * b(1) + a(2) * b(2)

    }

    override def plus(a: Float2, b: Float2): Float2 = {
      Float2(a(1) + b(1), a(2) + b(2))
    }

    override def negate(a:Float2): Float2 = {
      Float2(-a(1), -a(2))
    }

    override def times(a: Float2, k: Float): Float2 = {
      Float2(a(1) * k, a(2) * k)
    }

    override def zero: Float2 = {
      Float2(0F,0F)
    }



    override def create(coordinates: Float*): Float2 = {
      Float2(coordinates(0), coordinates(1))
    }

    override def get(v: Float2, i: Int): Float = v(i)

    override def x(v: Float2): Float = v(1)

    override def y(v: Float2): Float = v(2)

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Float2, b: Float2): Float2 = Float2(a.x * b.x, a.y * b.y)
  }

  */

}

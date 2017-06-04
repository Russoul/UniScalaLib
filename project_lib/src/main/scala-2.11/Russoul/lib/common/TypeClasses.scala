package Russoul.lib.common

import Russoul.lib.common.math.algebra.{ComplexOverField, Mat, Vec}
import Russoul.lib.common.math.linear.Mat4
import Russoul.lib.common.utils.Arr

import scala.collection.parallel.immutable
import scala.language.implicitConversions
import scala.math.Ordering

/**
  * Created by russoul on 18.05.17.
  */
object TypeClasses {
  trait Addable[@specialized A] {
    @inline def plus(x: A, y:A): A

    override def toString: String

    class AddableOps(lhs: A)(implicit ev: Addable[A]){
      @inline def +(rhs: A): A = ev.plus(lhs, rhs)
    }

    //implicit def mkAddableOps(lhs: A): AddableOps = new AddableOps(lhs)
  }


  object Addable {
    object Implicits{
      implicit def infixAddableOps[A](x: A)(implicit num: Addable[A]): Addable[A]#AddableOps = new num.AddableOps(x)
    }
  }

  trait CanBeNegated[@specialized A]{
    @inline def negate(x: A): A
  }

  trait Ordered[@specialized A] extends CanBeNegated[A] with Ordering[A]{
    @inline def abs(x:A): A = max(x, negate(x))
  }

  trait CommutativeGroup[@specialized A] extends Addable[A] with CanBeNegated[A]{
    
    @inline def zero: A


    @inline def minus(x: A, y: A): A = plus(x, negate(y))

    class CommutativeGroupOps(lhs: A)(implicit ev: CommutativeGroup[A]) extends AddableOps(lhs){
      @inline def -(rhs: A): A = ev.minus(lhs, rhs)
      @inline def unary_-(): A = ev.negate(lhs)

    }

    //implicit def mkCommutativeGroupOps(lhs: A): CommutativeGroupOps = new CommutativeGroupOps(lhs)

  }

  object CommutativeGroup {
    object Implicits{
      implicit def infixCommutativeGroupLikeOps[A](x: A)(implicit num: CommutativeGroup[A]): CommutativeGroup[A]#CommutativeGroupOps = new num.CommutativeGroupOps(x)


    }
  }

  trait Trig[@specialized A]{
    @inline def atan2(x:A, y:A) : A
    @inline def toRadians(x:A): A
    @inline def cos(x:A): A
    @inline def sin(x:A): A
    @inline def acos(x:A):A
  }

  trait Euclidean[@specialized A]
  {
    @inline def sqrt(x:A) : A
    @inline def pow(x: A, y:A): A
  }

  trait ConvertibleFromDouble[@specialized A]{
    @inline def fromDouble(x: Double) : A
  }

  

  trait Field[@specialized A] extends CommutativeGroup[A]{
    @inline def times(x: A, y: A): A
    @inline def inv(x: A): A
    @inline def one:A
    @inline def div(x: A, y: A): A = times(x, inv(y))



    class FieldOps(lhs: A)(implicit ev: Field[A]) extends CommutativeGroupOps(lhs){
      @inline def *(rhs: A) : A = ev.times(lhs, rhs)
      @inline def /(rhs: A) : A = ev.div(lhs, rhs)

    }

    //implicit def mkFieldOps(lhs: A): FieldOps = new FieldOps(lhs)
  }

  object Field {
    object Implicits{
      implicit def infixFieldLikeOps[@specialized A](x: A)(implicit num: Field[A]): Field[A]#FieldOps = new num.FieldOps(x)

      //TODO this results in a cast overhead !!!
      @inline implicit def to[@specialized A](x:Double)(con: ConvertibleFromDouble[A]): A = con.fromDouble(x)

      implicit class DoubleToSmth(x:Double){
        @inline def to[@specialized A](implicit con: ConvertibleFromDouble[A]): A = con.fromDouble(x)
      }
    }
  }

  abstract class VectorSpaceOverField[V,@specialized F] extends Field[F]{
    @inline def plus(a:V, b:V) : V
    @inline def negate(a:V):V
    @inline def times(a:V, k:F):V
    @inline def zero(dim:Int) : V

    @inline def div(a:V, k:F):V = times(a, inv(k))
    @inline def minus(a:V, b:V):V = plus(a, negate(b))

    @inline def create(coordinates: F*) : V

    @inline def get(v:V, i:Int) : F

    @inline @straight def x(v: V) : F
    @inline @straight def y(v: V) : F
    @inline @straight def z(v: V) : F
    @inline @straight def w(v: V) : F

    class VectorSpaceOps(lhs: V)(implicit ev: VectorSpaceOverField[V,F]){
      @inline def *(rhs: F) : V = ev.times(lhs, rhs)
      @inline def /(rhs: F) : V = ev.div(lhs, rhs)

      @inline def +(rhs: V) : V = ev.plus(lhs, rhs)
      @inline def -(rhs: V) : V = ev.minus(lhs, rhs)
      @inline def unary_-() : V = ev.negate(lhs)

      //starting from 1
      @inline def apply(i:Int): F = ev.get(lhs, i)
      @inline def x: F = ev.x(lhs)
      @inline def y: F = ev.y(lhs)
      @inline def z: F = ev.z(lhs)
      @inline def w: F = ev.w(lhs)
    }

  }
  object VectorSpaceOverField{
    object Implicits{
      implicit def infixVectorSpaceOps[V,@specialized F](x: V)(implicit num: VectorSpaceOverField[V,F]): VectorSpaceOverField[V,F]#VectorSpaceOps = new num.VectorSpaceOps(x)

    }
  }

  //used to better utilise implicits (being more concrete with what the matrix should be)
  trait MixinForMutables[A, B]{
    def mixin(obj: A) : A with B
    def mixinCopy(obj: A) : A with B
  }



  trait Gram
  trait CrossProduct

  //used to specify the area where the matrix may be used (great for implicits)
  class MatMixin[@specialized F, T] extends MixinForMutables[Mat[F], T]{
    /**
      *
      * @param mat
      * @return user shall not change contents of input "mat" as they point to the same Arr as the result
      *         faster then copying
      */
    def mixin(mat:  Mat[F] @mutable): Mat[F] with T =
    {
      new Mat[F](mat.rows, mat.columns, mat.ar) with T
    }

    /**
      *
      * @param mat
      * @return copy version of mixin, slower but the result is absolutely independent object
      */
    def mixinCopy(mat:  Mat[F] @mutable): Mat[F] with T =
    {
      new Mat[F](mat.rows, mat.columns, mat.ar.copy) with T
    }

  }


  abstract class EuclideanSpaceOverField[V,@specialized F] extends VectorSpaceOverField[V,F] with Trig[F] with Euclidean[F]{
    @inline def dotProduct(a:V, b:V, matGram:Mat[F] with Gram):F

    @inline def length(a:V, matGram:Mat[F] with Gram):F = sqrt(dotProduct(a,a, matGram))
    @inline def angle(a:V, b:V, matGram:Mat[F] with Gram):F = acos(div(div(dotProduct(a, b, matGram), length(a, matGram)), length(b, matGram)))

    @inline def canonicalBasis(dim:Int) : Arr[V]

    @inline def normalize(v: V, matGram : Mat[F] with Gram) : V = {
      val len = length(v, matGram)

      div(v, len)
    }

    class EuclideanSpaceOps(lhs: V)(implicit ev: EuclideanSpaceOverField[V,F]) extends VectorSpaceOps(lhs){
      @inline def *(rhs: V)(implicit matGram: Mat[F] with Gram) : F = ev.dotProduct(lhs, rhs, matGram)

      @inline def normalize()(implicit matGram: Mat[F] with Gram) : V = ev.normalize(lhs, matGram)
    }
  }
  object EuclideanSpaceOverField{
    object Implicits{
      implicit def infixEuclideanSpaceOps[V,@specialized F](x: V)(implicit num: EuclideanSpaceOverField[V,F]): EuclideanSpaceOverField[V,F]#EuclideanSpaceOps = new num.EuclideanSpaceOps(x)

    }
  }

  abstract class EuclideanSpace3OverField[V,@specialized F] extends EuclideanSpaceOverField[V,F]{

    /**
      * 
       * @param a
      * @param b
      * @param ijk [i,i] [i,j] [i,k]
      *            [j,i] [j,j] [j,k]
      *            [k,i] [k,j] [k,k]
      * @return
      */
    def crossProduct(a: V, b: V, ijk : Mat[V]) : V



    class EuclideanSpace3Ops(lhs: V)(implicit ev: EuclideanSpace3OverField[V,F]) extends EuclideanSpaceOps(lhs){
      @inline def ⨯(rhs: V)(implicit ijk: Mat[V]) : V = ev.crossProduct(lhs, rhs, ijk)
    }
  }
  object EuclideanSpace3OverField{
    object Implicits{
      implicit def infixEuclideanSpace3Ops[V,@specialized F](x: V)(implicit num: EuclideanSpace3OverField[V,F]): EuclideanSpace3OverField[V,F]#EuclideanSpace3Ops = new num.EuclideanSpace3Ops(x)
    }
  }

  abstract class EuclideanSpace2OverField[V,@specialized F] extends EuclideanSpaceOverField[V,F]



  //DEFINITIONS------------------------------------------------------

  trait IntIsCommutativeGroup extends CommutativeGroup[Int]{
    override def negate(x: Int): Int = -x
    override def zero: Int = 0
    override def plus(x: Int, y: Int): Int = x + y

    override def toString: String = {
      "Int"
    }

  }


  trait FloatIsTrig extends Trig[Float]{
    override def atan2(x: Float, y:Float): Float = Math.atan2(x,y).toFloat

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

  trait DoubleIsConvertibleFromDouble extends ConvertibleFromDouble[Double]{
    override def fromDouble(x: Double): Double = x
  }

  trait RealIsConvertibleFromDouble extends ConvertibleFromDouble[Real]{
    override def fromDouble(x: Real): Real = x
  }

  trait FloatIsEuclidean extends Euclidean[Float]{
    override def sqrt(x: Float): Float = Math.sqrt(x).toFloat
    override def pow(x: Float, y:Float): Float = Math.pow(x,y).toFloat
  }

  trait FloatIsField extends Field[Float]{
    override def times(x: Float, y: Float): Float = x * y
    override def inv(x: Float): Float = 1/x
    override val one: Float = 1F
    override def negate(x: Float): Float = -x
    override val zero: Float = 0F
    override def plus(x: Float, y: Float): Float = x + y
    override def toString: String = {
      "Float"
    }

  }


  trait DoubleIsTrig extends Trig[Double]{
    override def atan2(x: Double, y:Double): Double = Math.atan2(x,y)

    override def cos(x: Double): Double = Math.cos(x)
    override def sin(x: Double): Double = Math.sin(x)


    override def acos(x: Real): Real = Math.acos(x)

    override def toRadians(x: Double): Double = {
      Math.toRadians(x)
    }
  }

  trait DoubleIsEuclidean extends Euclidean[Double]{
    override def sqrt(x: Double): Double = Math.sqrt(x)
    override def pow(x: Double, y:Double): Double = Math.pow(x,y)
  }

  trait DoubleIsField extends Field[Double]{
    override def times(x: Double, y: Double): Double = x * y
    override def inv(x: Double): Double = 1/x
    override val one: Double = 1D
    override def negate(x: Double): Double = -x
    override val zero: Double = 0D
    override def plus(x: Double, y: Double): Double = x + y

    override def toString: String = {
      "Double"
    }
  }
  
  trait RealIsField extends Field[Real]{
    override def times(x: Real, y: Real): Real = x * y
    override def inv(x: Real): Real = 1/x
    override val one: Real = 1D
    override def negate(x: Real): Real = -x
    override val zero: Real = 0D
    override def plus(x: Real, y: Real): Real = x + y

    override def toString: String = {
      "Real"
    }
  }
  
  trait RealIsEuclidean extends Euclidean[Real]{
    override def sqrt(x: Real): Real = Math.sqrt(x)
    override def pow(x: Real, y:Real): Real = Math.pow(x,y)
  }

  trait RealIsTrig extends Trig[Real]{
    override def atan2(x: Real, y:Real): Real = Math.atan2(x,y)

    override def cos(x: Real): Real = Math.cos(x)
    override def sin(x: Real): Real = Math.sin(x)


    override def acos(x: Real): Real = Math.acos(x)

    override def toRadians(x: Real): Real = {
      Math.toRadians(x)
    }
  }

  trait RealOrdering extends Ordering[Real] {
    outer =>

    def compare(x: Real, y: Real) = java.lang.Double.compare(x, y)

    override def lteq(x: Real, y: Real): Boolean = x <= y
    override def gteq(x: Real, y: Real): Boolean = x >= y
    override def lt(x: Real, y: Real): Boolean = x < y
    override def gt(x: Real, y: Real): Boolean = x > y
    override def equiv(x: Real, y: Real): Boolean = x == y
    override def max(x: Real, y: Real): Real = Math.max(x, y)
    override def min(x: Real, y: Real): Real = Math.min(x, y)

    override def reverse: Ordering[Real] = new RealOrdering {
      override def reverse = outer
      override def compare(x: Real, y: Real) = outer.compare(y, x)

      override def lteq(x: Real, y: Real): Boolean = outer.lteq(y, x)
      override def gteq(x: Real, y: Real): Boolean = outer.gteq(y, x)
      override def lt(x: Real, y: Real): Boolean = outer.lt(y, x)
      override def gt(x: Real, y: Real): Boolean = outer.gt(y, x)
      override def min(x: Real, y: Real): Real = outer.max(x, y)
      override def max(x: Real, y: Real): Real = outer.min(x, y)
    }
  }


  implicit object IntIsCommutativeGroup extends IntIsCommutativeGroup with Ordering.IntOrdering
  implicit object FloatIsField extends FloatIsField with FloatIsTrig
    with FloatIsEuclidean with FloatIsConvertibleFromDouble with Ordering.FloatOrdering
  implicit object DoubleIsField extends DoubleIsField with DoubleIsTrig
    with DoubleIsEuclidean with DoubleIsConvertibleFromDouble
    with Ordering.DoubleOrdering

  implicit object RealIsField extends RealIsField with RealIsTrig
    with RealIsEuclidean with RealIsConvertibleFromDouble
    with RealOrdering

  trait ComplexIsField extends Field[ComplexOverField[Real]]{
    override def times(x: ComplexOverField[Real], y: ComplexOverField[Real]): ComplexOverField[Real] = x * y
    override def inv(x: ComplexOverField[Real]): ComplexOverField[Real] = one / x
    override val one: ComplexOverField[Real] = ComplexOverField(1D,0D)
    override def negate(x: ComplexOverField[Real]): ComplexOverField[Real] = -x
    override val zero: ComplexOverField[Real] = ComplexOverField(0D,0D)
    override def plus(x: ComplexOverField[Real], y: ComplexOverField[Real]): ComplexOverField[Real] = x + y

  }

  trait Double3IsField extends Field[(Double, Double, Double)]{
    override def times(x: (Double, Double, Double), y: (Double, Double, Double)): (Double, Double, Double) = (x._1 * y._1, x._2 * y._2, x._3 * y._3)
    override def inv(x: (Double, Double, Double)): (Double, Double, Double) = (1/x._1, 1/x._2, 1/x._3)
    override val one: (Double, Double, Double) = (1D,1D,1D)
    override def negate(x: (Double, Double, Double)): (Double, Double, Double) = (-x._1, -x._2, -x._3)
    override val zero: (Double, Double, Double) = (0D,0D,0D)
    override def plus(x: (Double, Double, Double), y: (Double, Double, Double)): (Double, Double, Double) = (x._1 + y._1, x._2 + y._2, x._3 + y._3)

    override def toString: String = {
      "Double3"
    }
  }

  implicit object Double3IsField extends Double3IsField


  abstract class Double3IsEuclideanSpace3OverDouble(implicit ev: Field[Double]) extends EuclideanSpace3OverField[(Double,Double,Double), Double]{
    override def dotProduct(a: (Double, Double, Double), b: (Double, Double, Double), gram: Mat[Double] with Gram): Double = {
      //TODO dim check ??? must be 3 !


      a._1 * b._1 * gram(0)(0) + a._1 * b._2 * gram(0)(1) + a._1 * b._3 * gram(0)(2) +
        a._2 * b._1 * gram(1)(0) + a._2 * b._2 * gram(1)(1) + a._2 * b._3 * gram(1)(2) +
        a._3 * b._1 * gram(2)(0) + a._3 * b._2 * gram(2)(1) + a._3 * b._3 * gram(2)(2)

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
    override def crossProduct(a: (Real, Real, Real), b: (Real, Real, Real), ijk: Mat[(Double, Double, Double)]): (Real, Real, Real) = {
      val i0 = times(ijk(0)(0), a._1 * b._1)
      val i1 = times(ijk(0)(1), a._1 * b._2)
      val i2 = times(ijk(0)(2), a._1 * b._3)
      val i3 = times(ijk(1)(0), a._2 * b._1)
      val i4 = times(ijk(1)(1), a._2 * b._2)
      val i5 = times(ijk(1)(2), a._2 * b._3)
      val i6 = times(ijk(2)(0), a._3 * b._1)
      val i7 = times(ijk(2)(1), a._3 * b._2)
      val i8 = times(ijk(2)(2), a._3 * b._3)
      
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

    override def zero(dim:Int): (Double, Double, Double) = {
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

    override def w(v: (Real, Real, Real)): Real = throw new Exception("operation undefined")


    override def create(coordinates: Double*): (Double, Double, Double) = {
      (coordinates(0), coordinates(1), coordinates(2))
    }




    def canonicalBasis(dim:Int):Arr[(Double, Double, Double)] = Arr((1D,0,0), (0,1D,0), (0,0,1D))
  }

  implicit object Double3IsEuclideanSpace3OverDouble extends Double3IsEuclideanSpace3OverDouble with DoubleIsField with DoubleIsTrig with DoubleIsConvertibleFromDouble with DoubleIsEuclidean with Ordering.DoubleOrdering

  abstract class Real3IsEuclideanSpace3OverReal(implicit ev: Field[Real]) extends EuclideanSpace3OverField[Real3, Real]{
    override def dotProduct(a: Real3, b: Real3, gram: Mat[Real] with Gram): Real = {
      //TODO dim check ??? must be 3 !
      

      a.x * b.x * gram(0)(0) + a.x * b.y * gram(0)(1) + a.x * b.z * gram(0)(2) +
        a.y * b.x * gram(1)(0) + a.y * b.y * gram(1)(1) + a.y * b.z * gram(1)(2) +
        a.z * b.x * gram(2)(0) + a.z * b.y * gram(2)(1) + a.z * b.z * gram(2)(2)

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
    override def crossProduct(a: Real3, b: Real3, ijk: Mat[Real3]): Real3 = {
      val i0 = times(ijk(0)(0), a.x * b.x)
      val i1 = times(ijk(0)(1), a.x * b.y)
      val i2 = times(ijk(0)(2), a.x * b.z)
      val i3 = times(ijk(1)(0), a.y * b.x)
      val i4 = times(ijk(1)(1), a.y * b.y)
      val i5 = times(ijk(1)(2), a.y * b.z)
      val i6 = times(ijk(2)(0), a.z * b.x)
      val i7 = times(ijk(2)(1), a.z * b.y)
      val i8 = times(ijk(2)(2), a.z * b.z)

      Real3(i0.x + i1.x+ i2.x + i3.x + i4.x + i5.x + i6.x+ i7.x + i8.x,
        i0.y + i1.y+ i2.y + i3.y + i4.y + i5.y + i6.y+ i7.y + i8.y,
        i0.z + i1.z+ i2.z + i3.z + i4.z + i5.z + i6.z+ i7.z + i8.z
      )
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

    override def zero(dim:Int): Real3 = {
      Real3(0D,0D,0D)
    }


    override def create(coordinates: Real*): Real3 = {
      Real3(coordinates(0), coordinates(1), coordinates(2))
    }


    override def get(v: Real3, i: Int): Real = v(i)


    override def x(v: Real3): Real = v.x

    override def y(v: Real3): Real = v.y

    override def z(v: Real3): Real = v.z

    override def w(v: Real3): Real = throw new Exception("operation undefined")

    def canonicalBasis(dim:Int):Arr[Real3] = Arr(Real3(1D,0D,0D), Real3(0D,1D,0D), Real3(0D,0D,1D))
  }

  implicit object Real3IsEuclideanSpace3OverReal extends Real3IsEuclideanSpace3OverReal with RealIsField with RealIsTrig with RealIsEuclidean with RealIsConvertibleFromDouble with RealOrdering


  abstract class Real2IsEuclideanSpace2OverReal(implicit ev: Field[Real]) extends EuclideanSpace2OverField[Real2, Real]{
    override def dotProduct(a: Real2, b: Real2, gram: Mat[Real] with Gram): Real = {
      //TODO dim check ??? must be 2 !

      

      a.x * b.x * gram(0)(0) + a.x * b.y * gram(0)(1)  +
        a.y * b.x * gram(1)(0) + a.y * b.y * gram(1)(1)

    }



    override def plus(a: Real2, b: Real2): Real2 = {
      Real2(a.x + b.x, a.y + b.y)
    }

    override def negate(a:Real2): Real2 = {
      Real2(-a.x, -a.y)
    }

    override def times(a: Real2, k: Real): Real2 = {
      Real2(a.x * k, a.y * k)
    }

    override def zero(dim:Int): Real2 = {
      Real2(0D,0D)
    }


    override def create(coordinates: Real*): Real2 = {
      Real2(coordinates(0), coordinates(1))
    }

    override def get(v: Real2, i: Int): Real = v(i)

    override def x(v: Real2): Real = v.x

    override def y(v: Real2): Real = v.y

    override def z(v: Real2): Real = throw new Exception("operation undefined")

    override def w(v: Real2): Real = throw new Exception("operation undefined")


    def canonicalBasis(dim:Int):Arr[Real2] = Arr(Real2(1D,0D), Real2(0D,1D))
  }

  implicit object Real3IsEuclideanSpace2OverReal extends Real2IsEuclideanSpace2OverReal with RealIsField with RealIsTrig with RealIsEuclidean with RealIsConvertibleFromDouble with RealOrdering

  abstract class VecIsEuclideanSpaceOverReal extends EuclideanSpaceOverField[Vec[Real], Real]{


    /**
      *
      * @param a will be set as row
      * @param b will be set as column
      * @param gram
      * @return
      */
    override def dotProduct(a: Vec[Real], b: Vec[Real], gram: Mat[Real] with Gram): Real = {

      (a.setAsRow() ⨯ gram ⨯ b.setAsColumn()).toScalar()
    }

    override def canonicalBasis(dim: Int): Arr[Vec[Real]] = {
      val res = Arr.ofSize[Vec[Real]](dim)
      for(i <- 0 until dim){
        val ar = new Arr[Real](dim)
        ar(i) = 1D
        res += new Vec(ar)
      }

      res
    }

    override def plus(a: Vec[Real], b: Vec[Real]): Vec[Real] = a + b

    override def negate(a: Vec[Real]): Vec[Real] = -a

    override def times(a: Vec[Real], k: Real): Vec[Real] = a * k

    override def zero(dim: Int): Vec[Real] = new Vec(Arr.ofSize(dim))

    override def create(coordinates: Real*): Vec[Real] = {
      val ar = new Arr[Real](coordinates.size)
      for(c <- coordinates) ar += c

      Vec(ar)
    }

    override def get(v: Vec[Real], i: Int): Real = v(i)

    override def x(v: Vec[Real]): Real = v.x

    override def y(v: Vec[Real]): Real = v.y

    override def z(v: Vec[Real]): Real = v.z

    override def w(v: Vec[Real]): Real = v.w
  }

  implicit object VecIsEuclideanSpaceOverReal extends VecIsEuclideanSpaceOverReal with RealIsField with RealIsTrig with RealIsEuclidean with RealIsConvertibleFromDouble with RealOrdering


}

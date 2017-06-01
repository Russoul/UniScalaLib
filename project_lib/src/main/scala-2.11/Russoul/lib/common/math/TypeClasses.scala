package Russoul.lib.common.math

import Russoul.lib.common.Real
import Russoul.lib.common.math.immutable.algebra.{ComplexOverField, Mat, Vec}
import Russoul.lib.common.utils.Arr

import scala.language.implicitConversions

/**
  * Created by russoul on 18.05.17.
  */
object TypeClasses {
  trait Addable[A] {
    @inline def plus(x: A, y:A): A

    override def toString: String

    class AddableOps(lhs: A)(implicit ev: Addable[A]){
      def +(rhs: A): A = ev.plus(lhs, rhs)
    }

    //implicit def mkAddableOps(lhs: A): AddableOps = new AddableOps(lhs)
  }

  object Addable {
    object Implicits{
      implicit def infixAddableOps[A](x: A)(implicit num: Addable[A]): Addable[A]#AddableOps = new num.AddableOps(x)
    }
  }

  trait CommutativeGroup[A] extends Addable[A] with Ordering[A]{
    @inline def negate(x: A): A
    @inline def zero: A

    @inline def abs(x:A): A = {
      val neg = negate(x)

      if(lt(x,neg)){
        neg
      }else
        x
    }

    @inline def minus(x: A, y: A): A = plus(x, negate(y))

    class CommutativeGroupOps(lhs: A)(implicit ev: CommutativeGroup[A]) extends AddableOps(lhs){
      def -(rhs: A): A = ev.minus(lhs, rhs)
      def unary_-(): A = ev.negate(lhs)

    }

    //implicit def mkCommutativeGroupOps(lhs: A): CommutativeGroupOps = new CommutativeGroupOps(lhs)

  }

  object CommutativeGroup {
    object Implicits{
      implicit def infixCommutativeGroupLikeOps[A](x: A)(implicit num: CommutativeGroup[A]): CommutativeGroup[A]#CommutativeGroupOps = new num.CommutativeGroupOps(x)


    }
  }

  trait Trig[A]{
    @inline def atan2(x:A, y:A) : A
    @inline def toRadians(x:A): A
    @inline def cos(x:A): A
    @inline def sin(x:A): A
    @inline def acos(x:A):A
  }

  trait Euclidean[A]
  {
    @inline def sqrt(x:A) : A
    @inline def pow(x: A, y:A): A
  }

  trait ConvertibleFromDouble[A]{
    @inline def fromDouble(x: Double) : A
  }

  trait Field[A] extends CommutativeGroup[A]{
    @inline def times(x: A, y: A): A
    @inline def inv(x: A): A
    @inline def one:A
    @inline def div(x: A, y: A): A = times(x, inv(y))



    class FieldOps(lhs: A)(implicit ev: Field[A]) extends CommutativeGroupOps(lhs){
      def *(rhs: A) : A = ev.times(lhs, rhs)
      def /(rhs: A) : A = ev.div(lhs, rhs)



    }

    //implicit def mkFieldOps(lhs: A): FieldOps = new FieldOps(lhs)
  }

  object Field {
    object Implicits{
      implicit def infixFieldLikeOps[A](x: A)(implicit num: Field[A]): Field[A]#FieldOps = new num.FieldOps(x)

      //TODO this results in a cast overhead !!!
      @inline implicit def to[A](x:Double)(con: ConvertibleFromDouble[A]): A = con.fromDouble(x)

      implicit class DoubleToSmth(x:Double){
        @inline def to[A](implicit con: ConvertibleFromDouble[A]): A = con.fromDouble(x)
      }
    }
  }

  abstract class VectorSpaceOverField[V,F](implicit ev: Field[F]) {
    @inline def plus(a:V, b:V) : V
    @inline def negate(a:V):V
    @inline def times(a:V, k:F):V
    @inline def zero(dim:Int) : V

    @inline def divide(a:V, k:F):V = times(a, ev.inv(k))
    @inline def minus(a:V, b:V):V = plus(a, negate(b))

  }

  abstract class EuclideanSpaceOverField[V,F](implicit ev: Field[F] with Trig[F] with Euclidean[F]) extends VectorSpaceOverField[V,F]{
    @inline def dotProduct(a:V, b:V, basis:Arr[V]):F

    @inline def length(a:V, basis:Arr[V]):F = ev.sqrt(dotProduct(a,a, basis))
    @inline def angle(a:V, b:V, basis:Arr[V]):F = ev.acos(ev.div(ev.div(dotProduct(a, b, basis), length(a, basis)), length(b, basis)))

    @inline def canonicalBasis(dim:Int) : Arr[V]
  }










  //DEFINITIONS------------------------------------------------------

  trait IntIsCommutativeGroup extends CommutativeGroup[Int]{
    override def negate(x: Int): Int = -x
    override def zero: Int = 0
    override def plus(x: Int, y: Int): Int = x + y

    override def toString: String = {
      "Int"
    }

  }

  //TODO make the only class IntN that can be c++ like templated
  trait Int2IsCommutativeGroup extends CommutativeGroup[Int]{

  }

  trait FloatIsTrig extends Trig[Float]{
    override def atan2(x: Float, y:Float): Float = math.atan2(x,y).toFloat

    override def cos(x: Float): Float = math.cos(x).toFloat
    override def sin(x: Float): Float = math.sin(x).toFloat


    override def acos(x: Float): Float = math.acos(x).toFloat

    override def toRadians(x: Float): Float = {
      math.toRadians(x).toFloat
    }
  }

  trait FloatIsConvertibleFromDouble extends ConvertibleFromDouble[Float]{
    override def fromDouble(x: Double): Float = x.toFloat
  }

  trait DoubleIsConvertibleFromDouble extends ConvertibleFromDouble[Double]{
    override def fromDouble(x: Double): Double = x
  }

  trait FloatIsEuclidean extends Euclidean[Float]{
    override def sqrt(x: Float): Float = math.sqrt(x).toFloat
    override def pow(x: Float, y:Float): Float = math.pow(x,y).toFloat
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
    override def atan2(x: Double, y:Double): Double = math.atan2(x,y)

    override def cos(x: Double): Double = math.cos(x)
    override def sin(x: Double): Double = math.sin(x)


    override def acos(x: Real): Real = math.acos(x)

    override def toRadians(x: Double): Double = {
      math.toRadians(x)
    }
  }

  trait DoubleIsEuclidean extends Euclidean[Double]{
    override def sqrt(x: Double): Double = math.sqrt(x)
    override def pow(x: Double, y:Double): Double = math.pow(x,y)
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



  implicit object IntIsCommutativeGroup extends IntIsCommutativeGroup with Ordering.IntOrdering
  implicit object FloatIsField extends FloatIsField with FloatIsTrig
    with FloatIsEuclidean with FloatIsConvertibleFromDouble with Ordering.FloatOrdering
  implicit object DoubleIsField extends DoubleIsField with DoubleIsTrig
    with DoubleIsEuclidean with DoubleIsConvertibleFromDouble
    with Ordering.DoubleOrdering

  trait ComplexIsField extends Field[ComplexOverField[Real]]{
    override def times(x: ComplexOverField[Real], y: ComplexOverField[Real]): ComplexOverField[Real] = x * y
    override def inv(x: ComplexOverField[Real]): ComplexOverField[Real] = one / x
    override val one: ComplexOverField[Real] = ComplexOverField(1D,0D)
    override def negate(x: ComplexOverField[Real]): ComplexOverField[Real] = -x
    override val zero: ComplexOverField[Real] = ComplexOverField(0D,0D)
    override def plus(x: ComplexOverField[Real], y: ComplexOverField[Real]): ComplexOverField[Real] = x + y

  }


  abstract class Double3IsEuclideanSpaceOverDouble(implicit ev: Field[Double]) extends EuclideanSpaceOverField[(Double,Double,Double), Double]{
    override def dotProduct(a: (Double, Double, Double), b: (Double, Double, Double), basis: Arr[(Double, Double, Double)]): Double = {
      //TODO dim check ??? must be 3 !

      def dot(i:Int, k:Int): Double ={
        var res = ev.zero
        val b1 = basis(i)
        val b2 = basis(k)

        res = ev.plus(res, ev.times(b1._1, b2._1))
        res = ev.plus(res, ev.times(b1._2, b2._2))
        res = ev.plus(res, ev.times(b1._3, b2._3))

        res
      }

      a._1 * b._1 * dot(0,0) + a._1 * b._2 * dot(0,1) + a._1 * b._3 * dot(0,2) +
        a._2 * b._1 * dot(1,0) + a._2 * b._2 * dot(1,1) + a._2 * b._3 * dot(1,2) +
        a._3 * b._1 * dot(2,0) + a._3 * b._2 * dot(2,1) + a._3 * b._3 * dot(2,2)

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

    def canonicalBasis(dim:Int):Arr[(Double, Double, Double)] = Arr((1D,0,0), (0,1D,0), (0,0,1D))
  }

  implicit object Double3IsEuclideanSpaceOverDouble extends Double3IsEuclideanSpaceOverDouble

  abstract class VecIsEuclideanSpaceOverReal extends EuclideanSpaceOverField[Vec[Real], Real]{

    /**
      *
      * @param a will be set as row
      * @param b will be set as column
      * @param basis
      * @return
      */
    override def dotProduct(a: Vec[Real], b: Vec[Real], basis: Arr[Vec[Real]]): Real = {
      def mat(): Mat[Real] =
      {
        val ar = new Arr[Real](basis.size * basis.size)
        val mat = Mat(basis.size, basis.size, ar)

        for(i <- 1 to basis.size){
          for(j <- 1 to basis.size){
            for(k <- 1 to basis.size){
              mat(i)(j) += basis(i-1)(k) * basis(j-1)(k)
            }
          }
        }

        mat
      }

      (a.setAsRow() ⨯ mat ⨯ b.setAsColumn()).toScalar()
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
  }


}

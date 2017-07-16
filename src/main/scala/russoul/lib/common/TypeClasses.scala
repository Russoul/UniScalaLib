package russoul.lib.common

import russoul.lib.common.math.algebra._
import russoul.lib.common.utils.Arr

import scala.language.{higherKinds, implicitConversions}
import scala.math.Ordering
import scala.reflect.ClassTag
import Implicits._
import russoul.lib.common.Ops.{CanonicalEuclideanSpaceOps, FieldOps}
import russoul.lib.common.StaticContainerTypeClasses.VecIsTensor1
import russoul.lib.common.TypeClasses.CanonicalEuclideanSpaceOverField
import shapeless.ops.nat
import shapeless.ops.nat.{Diff, GT, Sum, ToInt}
import shapeless.{<:!<, Nat, Succ}

/**
  * Created by russoul on 18.05.17.
  */
object TypeClasses {

  class OperationUnsupportedException(str: String) extends Exception(str)


  trait Container1[@tbsp T, Con]{
    def x(con: Con): T
  }
  trait Container2[@tbsp T, Con] extends Container1[T,Con]{
    def y(con: Con): T
  }
  trait Container3[@tbsp T, Con] extends Container2[T,Con]{
    def z(con: Con): T
  }
  trait Container4[@tbsp T, Con] extends Container3[T,Con]{
    def w(con: Con): T
  }

  trait ContainerAny[@tbsp T, Con] extends Container4[T,Con]{
    def apply(con: Con, i: Int) : T
    def size(con: Con) : Int
  }





  //TENSORS(just as data (scalars, vectors, matrices, 3d matrices, 4d matrices and so on))----------------------
  //TODO better name for this kind of stuff ? Real tensors must also provide a law according to which each 'data-tensor' is transformed (switching basis)

  //these guys do not provide a safe way to get index a 'data-tensor'
  //and they also do not provide any common operations on them
  //so algebraic representations(i.e AlgebraicVector, AlgebraicMatrix and so on) should be used as they provide the later
  //Tensors are used just to provide evidence that a particular type is a 'data-tensor'

  trait Tensor[@tbsp T, Dim <: Nat]
  trait Tensor0[@tbsp T] extends Tensor[T, Nat._0] // == scalar
  trait Tensor1[@tbsp T, Vec[_,_<: Nat], A1 <: Nat] extends Tensor[T, Nat._1]{ //== vector
    def make(args: T*)(implicit ev1: ToInt[A1]) : Vec[T, A1]
    def get(a: Vec[T, A1], i: Int) : T
  }
  trait Tensor2[@tbsp T, Mat[_,_<: Nat,_<: Nat],  A1 <: Nat, A2 <: Nat] extends Tensor[T, Nat._2]{ //== matrix
    def make(args: T*)(implicit ev1: ToInt[A1], ev2: ToInt[A2]) : Mat[T, A1, A2]
    def get(a: Mat[T, A1, A2], i: Int, j: Int) : T
  }
  //--------------------------------------------------------------------------------------



  //TODO to be removed

  /*class DefaultAlgebraicFactory[@tbsp T : ClassTag] extends AlgebraicTypeFactory[T, Vec, Mat]{

    override def makeVector[Size <: Nat : ToInt](args: T*): Vec[T, Size] = Vec[T,Size](args : _*)
    override def makeMatrix[Size <: Nat : ToInt](args: T*): Mat[T, Size] = Mat[T,Size](args : _*)

    override protected def get[Size <: Nat : ToInt](vec: Vec[T, Size], index: Int): T = vec(index)
    override protected def get[Size <: Nat : ToInt](mat: Mat[T, Size], i: Int, j: Int): T = mat(i,j)
  }*/

  abstract class AlgebraicTypeFactory[@tbsp T : ClassTag, Vec[_,_ <: Nat], Mat[_,_ <: Nat]]{

    def makeVector[Size <: Nat : ToInt](args: T*) : Vec[T, Size]
    def makeMatrix[Size <: Nat : ToInt](args: T*) : Mat[T, Size]

    //should not be used by user
    def get[Size <: Nat : ToInt](vec: Vec[T,Size], index: Int) : T
    def get[Size <: Nat : ToInt](mat: Mat[T,Size], i: Int, j: Int) : T
  }
  //TODO...............


  //The stuff below is algebra on tensors(once more: tensors here are just data with no functions or laws)

  //indices start from 0
  //compile time static collection
  //this is actually a valid representation of a vector from a vector space
  //size of a collection typeclassing this abstract class must be known at compile time
  //and size of any instance of this collection must be the same
  //we cant have Con as a higher kind because of https://issues.scala-lang.org/browse/SI-9227
  final class AlgebraicVector[@tbsp T : ClassTag, Vec[_,_<: Nat]]{

    //val factory: AlgebraicTypeFactory[T, Vec, Mat]

    //val tensor0 : Tensor0[T]
    //val tensor1 : Tensor1[T, Vec, Size] //add size to StaticVector ?

    @inline def get[Index <: Nat, Size <: Nat](vec: Vec[T,Size], i: Index)(implicit index : ToInt[Index],ev1: Size <:!< Index, tensor1: Tensor1[T, Vec, Size]) : T = tensor1.get(vec, index())

  }



  //indices start from 0
  //algebraic matrices
  final class AlgebraicSquareMatrix[@tbsp T : ClassTag, Vec[_,_ <: Nat], Mat[_,_ <: Nat,_ <: Nat]]{



    //protected val factory: AlgebraicTypeFactory[T, Vec, Mat]



    @inline def get[Size <: Nat, IndexI <: Nat, IndexJ <: Nat](mat: Mat[T,Size,Size],
                                                               i: IndexI,
                                                               j: IndexJ)(implicit ev1: Size <:!< IndexI,
                                                               toIntI: ToInt[IndexI], ev2: Size <:!< IndexJ,
                                                               toIntJ: ToInt[IndexJ],
                                                                          tensor2: Tensor2[T, Mat, Size, Size]) : T = {
      tensor2.get(mat, toIntI(), toIntJ())
    }


    //private dynamic variants
    private def row[Size <: Nat](mat: Mat[T,Size,Size], row: Int)(implicit sizeEv: ToInt[Size], tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T,Mat,Size,Size]) : Vec[T,Size] = {

      val size = sizeEv()

      val seq = new Array[T](size)

      for(i <- 0 until size){
        seq(i) = tensor2.get(mat, row, i)
      }

      tensor1.make(seq : _*)
    }

    private def column[Size <: Nat](mat: Mat[T,Size,Size], column: Int)(implicit sizeEv: ToInt[Size], tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T,Mat,Size,Size]) : Vec[T,Size] = {

      val size = sizeEv()

      val seq = new Array[T](size)

      for(i <- 0 until size){
        seq(i) = tensor2.get(mat, i, column)
      }

      tensor1.make(seq : _*)
    }


    //...

    @inline def row[Size <: Nat, Index <: Nat](mat: Mat[T,Size,Size], row: Index)(implicit ev1: Size <:!< Index, sizeEv: ToInt[Size], rowEv : ToInt[Index], tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T,Mat,Size,Size]) : Vec[T,Size] = {
      this.row[Size](mat, rowEv.apply())
    }

    @inline def column[Size <: Nat, Index <: Nat](mat: Mat[T,Size,Size], column: Index)(implicit ev1: Size <:!< Index, sizeEv: ToInt[Size], colEv : ToInt[Index], tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T,Mat,Size,Size]) : Vec[T,Size] = {
      this.column(mat, colEv.apply())
    }

    def transpose[Size <: Nat](mat: Mat[T,Size,Size])(implicit sizeEv: ToInt[Size], tensor2: Tensor2[T,Mat,Size,Size]) : Mat[T,Size,Size] = {
      val size = sizeEv()

      val seq = new Array[T](size)
      for(i <- 0 until size){
        for(j <- 0 until size){
          seq(i + j * size) = tensor2.get(mat, j, i)
        }
      }

      tensor2.make(seq: _*)

    }

    type Space[Size <: Nat] = CanonicalEuclideanSpaceOverField[Vec, T, Size]
    def matrixMultiplication[Size <: Nat](a: Mat[T,Size,Size], b: Mat[T,Size,Size])(implicit sizeEv: ToInt[Size], space: Space[Size],tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T, Mat, Size, Size]) : Mat[T,Size,Size] = {

      val size = sizeEv()

      val seq = new Array[T](size * size)

      for(i <- 0 until size){
        for(j <- 0 until size){


          seq(i + j * size) = row(a , i) dot column(b, j)
        }
      }

      tensor2.make(seq: _*)
    }


    //vector is supposed to be a row-vector
    def vectorMatrixMultiplication[Size <: Nat](a: Vec[T,Size], b: Mat[T,Size,Size])(implicit sizeEv: ToInt[Size], space: Space[Size], tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T, Mat, Size, Size]): Vec[T,Size] = {
      val size = sizeEv()

      val seq = new Array[T](size)

      for(i <- 0 until size){
        seq(i) = a dot column(b,i)
      }

      tensor1.make(seq: _*)
    }

    //vector is supposed to be a column-vector
    def matrixVectorMultiplication[Size <: Nat](a:Mat[T,Size,Size], b: Vec[T,Size])(implicit sizeEv: ToInt[Size], space: Space[Size], tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T, Mat, Size, Size]): Vec[T,Size] = {
      val size = sizeEv()

      val seq = new Array[T](size)

      for(i <- 0 until size){
        seq(i) = row(a, i) dot b
      }

      tensor1.make(seq: _*)
    }

  }



  //...........

  //also called Linear operator
  trait LinearMap[V[_,_ <: Nat], @tbsp F, Dim <: Nat, Space <: VectorSpaceOverField[V,F,Dim]]{

    implicit val space: Space
    implicit val scalar = space.scalar

    implicit val tensor1 = space.tensor1

    def map(a: V[F,Dim]): V[F,Dim]
  }

  trait BilinearMap[V[_,_ <: Nat], @tbsp F, Dim <: Nat, Space <: VectorSpaceOverField[V,F,Dim]]{

    implicit val space: Space
    implicit val scalar = space.scalar

    implicit val tensor1 = space.tensor1

    def map(a: V[F,Dim], b: V[F,Dim]): V[F,Dim]
  }

  trait CrossProductOverCanonicalEuclideanSpaceOverField[V[_,_<: Nat], @tbsp F] extends BilinearMap[V,F, Nat._3, CanonicalEuclideanSpaceOverField[V, F, Nat._3]]{

    override def map(a: V[F,Nat._3], b: V[F,Nat._3]): V[F,Nat._3] = {
      space.tensor1.make(a.y * b.z - b.y * a.z, -(a.x*b.z - b.x*a.z), a.x * b.y - b.x * a.y)
    }
  }

  trait TwoDimensionalVectorOrthoOperatorOverCanonicalEuclideanSpaceOverField[V[_,_ <: Nat], @tbsp F] extends LinearMap[V,F,Nat._2, CanonicalEuclideanSpaceOverField[V, F, Nat._2]]{
    override def map(a: V[F, Nat._2]): V[F, Nat._2] = {
      space.tensor1.make(-a.y, a.x)
    }
  }



  trait Addable[@tbsp A] {
    @inline def plus(x: A, y:A): A

    override def toString: String
  }


  trait MultiplicativeMonoid[@tbsp A]{
    def times(x : A, y: A) : A
    def one : A
  }

  trait CanBeNegated[@tbsp A]{
    @inline def negate(x: A): A
  }

  trait Orderable[@tbsp A] extends CanBeNegated[A] with Ordering[A]{
    @inline def abs(x:A): A = max(x, negate(x))
  }


  trait CommutativeAdditiveGroup[@tbsp A] extends Addable[A] with CanBeNegated[A]{
    @inline def zero: A
    @inline def minus(x: A, y: A): A = plus(x, negate(y))
  }

  trait Trig[@tbsp A]{
    @inline def atan2(x:A, y:A) : A
    @inline def toRadians(x:A): A
    @inline def cos(x:A): A
    @inline def sin(x:A): A
    @inline def acos(x:A):A
    def atan(x: A) : A
  }

  trait Euclidean[@tbsp A]
  {
    @inline def sqrt(x:A) : A
    @inline def pow(x: A, y:A): A
    def cbrt(x: A) : A
  }

  //means that its is dim is 1
  trait ConvertibleFromDouble[@tbsp A]{
    @inline def fromDouble(x: Double) : A
  }

  trait Ring[@tbsp A] extends CommutativeAdditiveGroup[A] with MultiplicativeMonoid[A]

  trait Field[@tbsp A] extends Ring[A]{
    @inline def inv(x: A): A
    @inline def div(x: A, y: A): A = times(x, inv(y))

  }

  //pseudo fields are not real fields, so not all algebraic rules are satisfied, used to make something like int a field
  trait PseudoField[@tbsp A] extends Field[A]

  //all algebraic properties are met, used to differ between `real` fields and `not real`, `pseudo` fields
  trait RealField[@tbsp A] extends Field[A]


  trait ModuleOverRing[V[_,_<: Nat], @tbsp R, Dim <: Nat] extends CommutativeAdditiveGroup[V[R,Dim]]{

    type Vector = V[R,Dim]


    implicit val scalarTag : ClassTag[R]
    implicit val dim : ToInt[Dim]
    implicit def scalar: Ring[R]
    def staticContainer: AlgebraicVector[R, V] //V must be static container
    def tensor1: Tensor1[R,V,Dim]



    override def zero: V[R, Dim] = {
      val seq = new Array[R](dim())

      var i = 0
      while(i < dim()){
        seq(i) = scalar.zero
        i += 1
      }

      tensor1.make(seq : _*)
    }

    override def plus(a: V[R, Dim], b: V[R, Dim]): V[R, Dim] = {
      val seq = new Array[R](dim())

      var i = 0
      while(i < dim()){
        seq(i) = tensor1.get(a, i) + tensor1.get(b, i)
        i += 1
      }

      tensor1.make(seq : _*)
    }

    override def negate(a: V[R, Dim]): V[R, Dim] = {
      val seq = new Array[R](dim())

      var i = 0
      while(i < dim()){
        seq(i) = -tensor1.get(a, i)
        i += 1
      }

      tensor1.make(seq : _*)
    }


    @inline def times(a:V[R,Dim], k:R): V[R,Dim] = {
      val seq = new Array[R](dim())

      var i = 0
      while(i < dim()){
        seq(i) = tensor1.get(a, i) * k
        i += 1
      }

      tensor1.make(seq : _*)
    }


    //TODO Modules in general do not have this operation !
    @inline def timesByElement(a: V[R,Dim], b: V[R,Dim]) : V[R,Dim] = {
      val seq = new Array[R](dim())

      var k = 0
      while(k < dim()){
        seq(k) = tensor1.get(a, k) * tensor1.get(b, k)
        k += 1
      }

      tensor1.make(seq : _*)
    }
  }



  trait VectorSpaceOverField[V[_,_<: Nat],@tbsp F, Dim <: Nat] extends ModuleOverRing[V,F,Dim]{

    override implicit def scalar: Field[F]
    @inline def div(a:V[F,Dim], k:F):V[F,Dim] = times(a, scalar.inv(k))

  }



  //TODO D E P R E C A T E D ---------------------------------------------------------------------------------------
 /* trait ElementOps2[@tbsp T]{
    @inline def x: T
    @inline def y: T
  }

  trait ElementOps3[@tbsp T]{
    @inline def x: T
    @inline def y: T
    @inline def z: T
  }

  trait ElementOps4[@tbsp T]{
    @inline def x: T
    @inline def y: T
    @inline def z: T
    @inline def w: T
  }*/


  //used to better utilise implicits (being more concrete with what the matrix should be)
 /* trait MixinForMutables[A, B]{
    def mixin(obj: A) : A with B
    def mixinCopy(obj: A) : A with B
  }
*/


  /*trait Gram
  trait CrossProduct

  //used to specify the area where the matrix may be used (great for implicits)
  class GramMixin[@tbsp F : ClassTag](implicit ev: Field[F]) extends MixinForMutables[Mat[F], Gram]{
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
  class CrossProductMixin[@tbsp F : ClassTag](implicit ev: Field[F]) extends MixinForMutables[Mat[F], CrossProduct]{
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

  trait Mat4Mult[V, @tbsp F]{
    def multM(v : V, mat : Mat4[F]) : V
  }

  trait Canonical2DimOrthoOp[V]{
    def ortho(a: V) : V
  }

  trait EuclideanSpaceOverField[V,@tbsp F, Dim <: Nat] extends VectorSpaceOverField[V,F,Dim]{

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


  */
  //TODO--------------------------------------------------------------------------------




  //using canonical basis
  trait CanonicalEuclideanSpaceOverField[V[_,_<: Nat],@tbsp F, Dim <: Nat] extends VectorSpaceOverField[V,F,Dim] {

    override implicit def scalar: Field[F] with Trig[F] with Euclidean[F]

    @inline def dotProduct(a: V[F,Dim], b: V[F,Dim]): F = {
      var res = scalar.zero

      var i = 0
      while(i < dim()){
        res += tensor1.get(a, i) * tensor1.get(b, i)
        i += 1
      }

      res
    }

    @inline def squaredLength(a: V[F,Dim]): F = dotProduct(a, a)

    @inline def length(a: V[F,Dim]): F = scalar.sqrt(dotProduct(a, a))

    @inline def angle(a: V[F,Dim], b: V[F,Dim]): F = scalar.acos(scalar.div(scalar.div(dotProduct(a, b), length(a)), length(b)))

    @inline def normalize(v: V[F,Dim]): V[F,Dim] = {
      val len = length(v)
      div(v, len)
    }

  }

  //TODO DEFINITIONS------------------------------------------------------


  trait IntIsAddable extends Addable[Int]{
    override def plus(x: Int, y: Int): Int = x + y
  }

  trait IntIsCommutativeAdditiveGroup extends CommutativeAdditiveGroup[Int]{
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

  /*trait IntIsRing extends IntIsCommutativeAdditiveGroup with Ring[Int]{
    override def times(x: Int, y: Int): Int = x * y
    override def one: Int = 1
  }*/

  class IntIsFullPseudoField extends IntIsPseudoField with IntIsOrderable


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


  trait FloatIsField extends RealField[Float]{
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

  trait DoubleIsField extends RealField[Double]{
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

  trait IntIsPseudoField extends PseudoField[Int]{
    override def inv(x: Int): Int = 1 / x //this part breaks the rules

    override def zero: Int = 0

    override def times(x: Int, y: Int): Int = x * y

    override def one: Int = 1

    override def plus(x: Int, y: Int): Int = x + y

    override def negate(x: Int): Int = -x
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


  class VecIsCanonicalEuclideanSpaceOverField[@tbsp F : ClassTag, Dim <: Nat](field : Field[F] with Trig[F] with Euclidean[F])(implicit evDim: ToInt[Dim]) extends CanonicalEuclideanSpaceOverField[Vec, F, Dim]{
    override implicit def scalar: Field[F] with Trig[F] with Euclidean[F] = field
    override def staticContainer: AlgebraicVector[F, Vec] = new AlgebraicVector[F, Vec]
    override def tensor1: Tensor1[F, Vec, Dim] = new VecIsTensor1[F,Dim]()


    override val scalarTag: ClassTag[F] = implicitly[ClassTag[F]]
    override val dim: ToInt[Dim] = evDim
  }

  class Vec3HasCrossProduct[@tbsp F] extends CrossProductOverCanonicalEuclideanSpaceOverField[Vec, F]{
    override implicit val space: CanonicalEuclideanSpaceOverField[Vec, F, Nat._3] = implicitly[CanonicalEuclideanSpaceOverField[Vec, F, Nat._3]]
  }

  class Vec2HasOrtho[@tbsp F] extends TwoDimensionalVectorOrthoOperatorOverCanonicalEuclideanSpaceOverField[Vec, F]{
    override implicit val space: CanonicalEuclideanSpaceOverField[Vec, F, Nat._2] = implicitly[CanonicalEuclideanSpaceOverField[Vec, F, Nat._2]]
  }


  //Those are just normal containers with dynamic sizes (not known at compile time, moreover size cannot change across instances of collection even at compile time)
  class Tuple2IsContainer2[@tbsp T] extends Container2[T, (T,T)]{
    override def x(v: (T,T)): T = v._1

    override def y(v: (T,T)): T = v._2
  }
  class Tuple3IsContainer3[@tbsp T] extends Container3[T, (T,T,T)]{
    override def x(v: (T,T,T)): T = v._1

    override def y(v: (T,T,T)): T = v._2

    override def z(v: (T,T,T)): T = v._3
  }
  class Tuple4IsContainer4[@tbsp T] extends Container4[T, (T,T,T,T)]{
    override def x(v: (T,T,T,T)): T = v._1

    override def y(v: (T,T,T,T)): T = v._2

    override def z(v: (T,T,T,T)): T = v._3

    override def w(v: (T,T,T,T)): T = v._4
  }
  class Vec2IsContainer2[@tbsp T] extends Container2[T, Vec2[T]]{
    override def x(v: Vec2[T]): T = v(0)

    override def y(v: Vec2[T]): T = v(1)
  }
  class Vec3IsContainer3[@tbsp T] extends Container3[T, Vec3[T]]{
    override def x(v: Vec3[T]): T = v(0)

    override def y(v: Vec3[T]): T = v(1)

    override def z(v: Vec3[T]): T = v(2)
  }
  class Vec4IsContainer4[@tbsp T] extends Container4[T, Vec4[T]]{
    override def x(v: Vec4[T]): T = v(0)

    override def y(v: Vec4[T]): T = v(1)

    override def z(v: Vec4[T]): T = v(2)

    override def w(v: Vec4[T]): T = v(3)
  }
  class ArrayIsContainerAny[@tbsp T] extends ContainerAny[T, Array[T]]{
    override def x(v: Array[T]): T = v(0)

    override def y(v: Array[T]): T = v(1)

    override def z(v: Array[T]): T = v(2)

    override def w(v: Array[T]): T = v(3)

    override def apply(con: Array[T], i: Int): T = con(i)

    override def size(con: Array[T]): Int = con.size
  }



  /*class Int2IsModuleOverInt extends ModuleOverRing[Int2, Int,Nat._2]{


    override implicit def staticContainer: StaticVector[Int, Int2, Nat._2] = new Vec2IsStaticVector[Int]

    override implicit val scalar: Ring[Int] = new IntIsFullRing

    override def plus(a: Int2, b: Int2): Int2 = Int2(a.x + b.x, a.y + b.y)

    override def negate(a: Int2): Int2 = Int2(-a.x, -a.y)

    override def times(a: Int2, k: Int): Int2 = Int2(a.x * k, a.y * k)

    override def zero: Int2 = Int2(0,0)



    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Int2, b: Int2): Int2 = Int2(a.x * b.x, a.y * b.y)
  }


  class Int3IsModuleOverInt extends ModuleOverRing[Int3, Int, Nat._3]{


    override implicit def staticContainer: StaticVector[Int, Int3, _root_.shapeless.Nat._3] = new Vec3IsStaticVector[Int]

    override implicit val scalar: Ring[Int] = new IntIsFullRing

    override def plus(a: Int3, b: Int3): Int3 = Int3(a.x + b.x, a.y + b.y, a.z + b.z)

    override def negate(a: Int3): Int3 = Int3(-a.x, -a.y, -a.z)

    override def times(a: Int3, k: Int): Int3 = Int3(a.x * k, a.y * k, a.z * k)

    override def zero: Int3 = Int3(0,0,0)

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Int3, b: Int3): Int3 = Int3(a.x * b.x, a.y * b.y, a.z * b.z)
  }


  class Int4IsModuleOverInt extends ModuleOverRing[Int4, Int, Nat._4]{


    override implicit def staticContainer: StaticVector[Int, Int4, _root_.shapeless.Nat._4] = new Vec4IsStaticVector[Int]

    override implicit val scalar: Ring[Int] = new IntIsFullRing

    override def plus(a: Int4, b: Int4): Int4 = Int4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)

    override def negate(a: Int4): Int4 = Int4(-a.x, -a.y, -a.z, -a.w)

    override def times(a: Int4, k: Int): Int4 = Int4(a.x * k, a.y * k, a.z * k, a.w * k)

    override def zero: Int4 = Int4(0,0,0,0)

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Int4, b: Int4): Int4 = Int4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
  }*/




  //TODO DEPRECATED

  /*class VecIsCanonicalEuclideanSpaceOverReal(final val dimensions: Int) extends CanonicalEuclideanSpaceOverField[Vec[Real], Real]{



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
  }*/


  //DEPRECATED, only static collections are ok
  /*class ArrayIsCanonicalEuclideanSpaceOverField[@tbsp F : ClassTag](final val dimensions: Int, field: Field[F] with Trig[F] with Euclidean[F]) extends CanonicalEuclideanSpaceOverField[Array[F], F]{



    override implicit val scalar: Field[F] with Trig[F] with Euclidean[F] = field

    /**
      *
      * @param a will be set as row
      * @param b will be set as column
      * @return
      */
    override def dotProduct(a: Array[F], b: Array[F]): F = {

      var x = scalar.zero
      var k = 0
      while(k < dimensions){
        x += a(k) * b(k)
        k += 1
      }

      x
    }

    override def plus(a: Array[F], b: Array[F]): Array[F] = {
      val res = new Array[F](dimensions)

      for(i <- 0 until dimensions){
        res(i) = a(i) + b(i)
      }

      res
    }

    override def negate(a: Array[F]): Array[F] = {
      val res = new Array[F](dimensions)

      for(i <- 0 until dimensions){
        res(i) *= -scalar.one
      }

      res
    }

    override def times(a: Array[F], k: F): Array[F] = {
      val res = new Array[F](dimensions)

      for(i <- 0 until dimensions){
        res(i) = a(i) * k
      }

      res
    }

    override def zero: Array[F] = new Array[F](dimensions) //TODO

    override def create(coordinates: F*): Array[F] = {
      val ar = new Array[F](dimensions)
      var i = 0
      while(i < ar.size){
        ar(i) = coordinates(i)
        i += 1
      }

      ar
    }

    override def get(v: Array[F], i: Int): F = v(i)

    /**
      *
      * @param a
      * @param b
      * @return by element product
      */
    override def timesByElement(a: Array[F], b: Array[F]): Array[F] = {
      val res = new Array[F](dimensions)

      for(i <- 0 until dimensions){
        res(i) = a(i) * b(i)
      }

      res
    }

    override def x(v: Array[F]) = v(0)

    override def y(v: Array[F]) = v(1)

    override def z(v: Array[F]) = v(2)

    override def w(v: Array[F]) = v(3)
  }*/

  /*class Vec3IsCanonicalEuclideanSpaceOverField[@tbsp T : ClassTag](field: Field[T] with Trig[T] with Euclidean[T]) extends CanonicalEuclideanSpaceOverField[Vec3[T], T, Nat._3] with CrossProductOverCanonicalEuclideanSpaceOverField[Vec3[T], T]{


    override implicit val space: CanonicalEuclideanSpaceOverField[Vec3[T], T, Nat._3] = this
    override implicit val scalar: Field[T] with Trig[T] with Euclidean[T] = field

    override def dotProduct(a: Vec3[T], b: Vec3[T]): T = a.x * b.x + a.y * b.y + a.z * b.z


    override implicit def staticContainer: StaticVector[T, Vec3[T], Nat._3] = new Vec3IsStaticVector[T]


    /*val V4 = new Vec4IsCanonicalEuclideanSpaceOverField[T](field)
    override def multM(v: Vec3[T], m: Mat4[T]): Vec3[T] = {
      val v4 = Vec4[T](x(v), y(v), z(v), scalar.zero)
      Vec3[T](V4.dotProduct(v4, m.column(1)), V4.dotProduct(v4, m.column(2)), V4.dotProduct(v4, m.column(3)))
    }*/


    override def times(a: Vec3[T], k: T): Vec3[T] = Vec3[T](a.x * k, a.y * k, a.z * k)


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

  class Vec4IsCanonicalEuclideanSpaceOverField[@tbsp T : ClassTag](field: Field[T] with Trig[T] with Euclidean[T]) extends CanonicalEuclideanSpaceOverField[Vec4[T], T, Nat._4]{



    override implicit def staticContainer: StaticVector[T, Vec4[T], _root_.shapeless.Nat._4] = new Vec4IsStaticVector[T]

    override implicit val scalar: Field[T] with Trig[T] with Euclidean[T] = field

    override def dotProduct(a: Vec4[T], b: Vec4[T]): T = a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w


    /*override def multM(v: Vec4[T], m: Mat4[T]): Vec4[T] = {

      Vec4[T](dotProduct(v, m.column(1)), dotProduct(v, m.column(2)), dotProduct(v, m.column(3)), dotProduct(v, m.column(4)))
    }*/


    override def times(a: Vec4[T], k: T): Vec4[T] = Vec4[T](a.x * k, a.y * k, a.z * k, a.w * k)


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

  class Vec2IsCanonicalEuclideanSpaceOverField[@tbsp T : ClassTag](field: Field[T] with Trig[T] with Euclidean[T]) extends CanonicalEuclideanSpaceOverField[Vec2[T], T, Nat._2] with TwoDimensionalVectorOrhoOperatorOverCanonicalEuclideanSpaceOverField[Vec2[T], T]{


    override implicit val space: CanonicalEuclideanSpaceOverField[Vec2[T], T, Nat._2] = this
    override implicit val scalar: Field[T] with Trig[T] with Euclidean[T] = field

    override def dotProduct(a: Vec2[T], b: Vec2[T]): T = a.x * b.x + a.y * b.y


    override implicit def staticContainer: StaticVector[T, Vec2[T], Nat._2] = new Vec2IsStaticVector[T]

    override def times(a: Vec2[T], k: T): Vec2[T] = Vec2[T](a.x * k, a.y * k)


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






  */






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

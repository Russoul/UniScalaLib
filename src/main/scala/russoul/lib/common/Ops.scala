package russoul.lib.common

import scala.language.implicitConversions
import scala.language.experimental.macros
import russoul.lib.common.TypeClasses._
import shapeless.Nat
import shapeless.ops.nat.{GT, LT, ToInt}
import Nat._
import Implicits._

import scala.reflect.ClassTag
import russoul.lib.macros.Enricher


/**
  * Created by russoul on 13.06.2017.
  */
object Ops {

  class AddableOps[@tbsp A](lhs: A)(implicit ev: Addable[A]){
    @inline def +(rhs: A): A = macro Enricher.binop[A, A]
  }

  class OrderableOps[@tbsp A](lhs: A)(implicit ev: Orderable[A]){
    @inline def >(rhs: A) : Boolean = macro Enricher.binop[A, A]
    @inline def <(rhs: A) : Boolean = macro Enricher.binop[A, A]
    @inline def >=(rhs: A) : Boolean = macro Enricher.binop[A, A]
    @inline def <=(rhs: A) : Boolean = macro Enricher.binop[A, A]
    @inline def ===(rhs: A) : Boolean = macro Enricher.binop[A, A]
    @inline def =!=(rhs: A) : Boolean = macro Enricher.binop[A, A]
  }


  class MultiplicativeMonoidOps[@tbsp A](lhs: A)(implicit ev: MultiplicativeMonoid[A]){
    @inline def *(rhs: A) : A = macro Enricher.binop[A, A]
  }


  class CommutativeGroupOps[@tbsp A](lhs: A)(implicit ev: CommutativeAdditiveGroup[A]){
    @inline def -(rhs: A): A = macro Enricher.binop[A, A]
    @inline def unary_-(): A = macro Enricher.unop[A]
  }

  class FieldOps[@tbsp A](lhs: A)(implicit ev: Field[A]){
    @inline def /(rhs: A) : A = macro Enricher.binop[A, A]
  }


  class Tensor1Ops[@tbsp T, Vec[_,_<: Nat], Size <: Nat](lhs : Vec[T,Size])(implicit ev : Tensor1[T,Vec,Size]){
    @inline def _0(implicit ev1: GT[Size, Nat._0]) = macro Enricher.unopWithEv2[GT[Size, Nat._0], T]
    @inline def _1(implicit ev1: GT[Size, Nat._1]) = macro Enricher.unopWithEv2[GT[Size, Nat._1], T]
    @inline def _2(implicit ev1: GT[Size, Nat._2]) = macro Enricher.unopWithEv2[GT[Size, Nat._2], T]
    @inline def _3(implicit ev1: GT[Size, Nat._3]) = macro Enricher.unopWithEv2[GT[Size, Nat._3], T]


  }

  class StaticVectorOps[@tbsp T : ClassTag, Vec[_,_<: Nat], Size <: Nat : ToInt](lhs: Vec[T,Size])(implicit ev: AlgebraicVector[T,Vec], tensor1: Tensor1[T,Vec,Size]){
    @inline def apply[N <: Nat](nat : N)(implicit ev1 : GT[Size, N], ev2 : ToInt[N]) = ev.get(lhs, nat)

    @inline def toArray : Array[T] = {
      val size = ev.size(lhs)
      val ar = new Array[T](size)

      for(i <- 0 until size){
        ar(i) = tensor1.get(lhs, i)
      }

      ar
    }

  }

  class CrossProductOps[@tbsp T, Vec[_,_<: Nat]](lhs: Vec[T, Nat._3])(implicit ev: CrossProductOverCanonicalEuclideanSpaceOverField[Vec, T]){
    @inline def ⨯(rhs: Vec[T, Nat._3]) = macro Enricher.binop[Vec[T, Nat._3], Vec[T, Nat._3]]
    @inline def cross(rhs: Vec[T, Nat._3]) = macro Enricher.binop[Vec[T, Nat._3], Vec[T, Nat._3]]
  }

  class OrthoOps[@tbsp T, Vec[_,_<: Nat]](lhs: Vec[T, Nat._2]) (implicit ev: TwoDimensionalVectorOrthoOperatorOverCanonicalEuclideanSpaceOverField[Vec,T]){
    @inline def ⟂() = macro Enricher.unop[Vec[T, Nat._2]]
    @inline def ortho() = macro Enricher.unop[Vec[T, Nat._2]]
  }

  class SquareMatrixOpsVectorFirst[@tbsp T, Vec[_,_ <: Nat], Mat[_,_ <: Nat,_<: Nat], Size <: Nat : ToInt](lhs: Vec[T,Size])(implicit ev: AlgebraicSquareMatrix[T,Vec,Mat], space : CanonicalEuclideanSpaceOverField[Vec,T,Size], tensor1: Tensor1[T,Vec,Size],tensor2: Tensor2[T, Mat, Size, Size]){
    def *(rhs: Mat[T,Size,Size]) = ev.vectorMatrixMultiplication(lhs, rhs)
  }

  class SquareMatrixOps[@tbsp T, Vec[_,_<: Nat], Mat[_,_ <: Nat,_ <: Nat], Size <: Nat : ToInt](lhs: Mat[T,Size,Size])(implicit ev: AlgebraicSquareMatrix[T,Vec,Mat], space : CanonicalEuclideanSpaceOverField[Vec,T,Size], tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T,Mat,Size,Size]){
    def ⨯(rhs: Mat[T,Size,Size]) = ev.matrixMultiplication(lhs, rhs)
    def mult(rhs: Mat[T,Size,Size]) = ev.matrixMultiplication(lhs, rhs) //same as ⨯
    def *(rhs: Vec[T,Size]) = ev.matrixVectorMultiplication(lhs, rhs)
    def transpose() : Mat[T,Size,Size] = ev.transpose(lhs)
  }

  class ModuleOpsCommon[V[_,_<: Nat],@tbsp R, Dim <: Nat](lhs: V[R,Dim])(implicit ev: Module[V,R,Dim]){

    @inline def *(rhs: R) : V[R,Dim] = macro Enricher.binop[V[R,Dim], V[R,Dim]]
    @inline def ⊗(rhs: V[R,Dim]) : V[R,Dim] = macro Enricher.binop[V[R,Dim], V[R,Dim]]
    @inline def elem(rhs: V[R,Dim]) : V[R,Dim] = macro Enricher.binop[V[R,Dim], V[R,Dim]]

    @inline def x(implicit ev1: GT[Dim, Nat._0]) = macro Enricher.unopWithEv2[GT[Dim, Nat._0], R]
    @inline def y(implicit ev1: GT[Dim, Nat._1]) = macro Enricher.unopWithEv2[GT[Dim, Nat._1], R]
    @inline def z(implicit ev1: GT[Dim, Nat._2]) = macro Enricher.unopWithEv2[GT[Dim, Nat._2], R]
    @inline def w(implicit ev1: GT[Dim, Nat._3]) = macro Enricher.unopWithEv2[GT[Dim, Nat._3], R]

  }


  class ModuleOps2[V[_,_<: Nat],@tbsp R](lhs: V[R,Nat._2])(implicit ev: Module[V,R,Nat._2]){
  }

  class ModuleOps3[V[_,_<: Nat],@tbsp R](lhs: V[R,Nat._3])(implicit ev: Module[V,R,Nat._3]){
  }

  class ModuleOps4[V[_,_<: Nat],@tbsp R](lhs: V[R,Nat._4])(implicit ev: Module[V,R,Nat._4]){
    @inline def xyz(implicit t1: Tensor1[R,V,_3], tensor1: Tensor1[R,V,Nat._4]) = t1.make(tensor1.get(lhs, 0),tensor1.get(lhs, 1),tensor1.get(lhs, 2))
  }

  class Container1Ops[@tbsp T, Con](lhs: Con)(implicit ev: Container1[T,Con]){
    @inline def x: T = ev.x(lhs)
  }
  class Container2Ops[@tbsp T, Con2](lhs: Con2)(implicit ev: Container2[T,Con2]){
    //@inline def x: T = ev.x(lhs)
    @inline def y: T = ev.y(lhs)
  }
  class Container3Ops[@tbsp T, Con3](lhs: Con3)(implicit ev: Container3[T,Con3]){
    //@inline def x: T = ev.x(lhs)
    //@inline def y: T = ev.y(lhs)
    @inline def z: T = ev.z(lhs)
  }
  class Container4Ops[@tbsp T, Con4](lhs: Con4)(implicit ev: Container4[T,Con4]){
    //@inline def x: T = ev.x(lhs)
    //@inline def y: T = ev.y(lhs)
    //@inline def z: T = ev.z(lhs)
    @inline def w: T = ev.w(lhs)
  }
  class ContainerAnyOps[@tbsp T, Con](lhs: Con)(implicit ev: ContainerAny[T,Con]){
    /*@inline def x: T = ev.x(lhs)
    @inline def y: T = ev.y(lhs)
    @inline def z: T = ev.z(lhs)
    @inline def w: T = ev.w(lhs)*/
    @inline def apply(i: Int): T = ev.apply(lhs,i)
    @inline def size(): Int = ev.size(lhs)
  }

  class VectorSpaceOps[V[_,_<: Nat],@tbsp F, Dim <: Nat](lhs: V[F,Dim])(implicit ev: VectorSpaceOverField[V,F,Dim]){
    @inline def /(rhs: F) : V[F,Dim] = macro Enricher.binop[V[F,Dim], V[F,Dim]]
  }




  /*class EuclideanSpaceOps[V,@tbsp F](lhs: V)(implicit ev: EuclideanSpaceOverField[V,F,_]){
    @inline def ⋅(rhs: V)(implicit matGram: Mat[F] with Gram) : F = ev.dotProduct(lhs, rhs, matGram)
    @inline def dot(rhs: V)(implicit matGram: Mat[F] with Gram) : F = ev.dotProduct(lhs, rhs, matGram)
    @inline def normalize()(implicit matGram: Mat[F] with Gram) : V = ev.normalize(lhs, matGram)
    @inline def squaredLength()(implicit matGram: Mat[F] with Gram) : F = ev.squaredLength(lhs, matGram)
  }*/


  class CanonicalEuclideanSpaceOps[V[_,_<: Nat],@tbsp F, Dim <: Nat](lhs: V[F, Dim])(implicit ev: CanonicalEuclideanSpaceOverField[V,F,Dim]){
    @inline def ⋅(rhs: V[F,Dim]) : F = ev.dotProduct(lhs, rhs)
    @inline def dot(rhs: V[F,Dim]) : F = ev.dotProduct(lhs, rhs)
    //can't use * operator because of JVM type erasure (we already have this operator in Field[F] both are erased to Object => Object)
    @inline def normalize() : V[F,Dim] = ev.normalize(lhs)
    @inline def squaredLength() : F = ev.squaredLength(lhs)
    @inline def length() : F = ev.scalar.sqrt(ev.squaredLength(lhs))
  }

  /*class CanonicalCrossProductOps[V](lhs: V)(implicit ev: CanonicalCrossProductOp[V]){
    def ⨯(rhs: V) = ev.crossProduct(lhs, rhs)
  }

  class Canonical2DimCrossProductOps[V](lhs: V)(implicit ev: Canonical2DimOrthoOp[V]){
    def ⟂() = ev.ortho(lhs)
  }

  class Mat4MultOps[V](lhs: V){
    def ⨯[@tbsp F](rhs: Mat4[F])(implicit ev: Mat4Mult[V,F]): V = ev.multM(lhs, rhs)
  }*/



  trait AddableOpsImplicits{
    implicit def infixAddableOps[@tbsp A](x: A)(implicit num: Addable[A])= new AddableOps[A](x)
  }

  trait OrderableOpsImplicits{
    implicit def infixOrderableOps[@tbsp A](x: A)(implicit num: Orderable[A]) = new OrderableOps[A](x)
  }

  trait MultiplicativeMonoidImplicits{
    implicit def infixMultiplicativeMonoidOps[@tbsp A](x: A)(implicit num: MultiplicativeMonoid[A]) = new MultiplicativeMonoidOps[A](x)
  }

  trait CommutativeGroupImplicits{
    implicit def infixCommutativeGroupLikeOps[@tbsp A](x: A)(implicit num: CommutativeAdditiveGroup[A]) = new CommutativeGroupOps[A](x)
  }

  trait FieldImplicits{
    implicit def infixFieldLikeOps[@tbsp A](x: A)(implicit num: Field[A]) = new FieldOps[A](x)
  }

  object FieldImplicits extends FieldImplicits

  trait ModuleOverRingImplicits{
    implicit def infixModuleOps[V[_,_<: Nat],@tbsp R, Dim <: Nat](x: V[R,Dim])(implicit num: Module[V,R,Dim]) = new ModuleOpsCommon[V,R,Dim](x)
    implicit def infixModuleOps2[V[_,_<: Nat],@tbsp R](x: V[R,Nat._2])(implicit num: Module[V,R,Nat._2]) = new ModuleOps2[V,R](x)
    implicit def infixModuleOps3[V[_,_<: Nat],@tbsp R](x: V[R,Nat._3])(implicit num: Module[V,R,Nat._3]) = new ModuleOps3[V,R](x)
    implicit def infixModuleOps4[V[_,_<: Nat],@tbsp R](x: V[R,Nat._4])(implicit num: Module[V,R,Nat._4]) = new ModuleOps4[V,R](x)
  }


  trait TensorImplicits{
    implicit def infixTensor1Ops[V[_,_<: Nat],@tbsp T , A1 <: Nat](x : V[T,A1])(implicit t1 : Tensor1[T,V,A1]) = new Tensor1Ops[T,V,A1](x)
  }

  trait VectorSpaceOverFieldImplicits{
    implicit def infixVectorSpaceOps[V[_,_<: Nat],@tbsp F,Dim <: Nat](x: V[F,Dim])(implicit ev: VectorSpaceOverField[V,F,Dim]) = new VectorSpaceOps[V,F,Dim](x)
  }

  trait MatrixImplicits{
    implicit def infixMatrixOps[@tbsp T, Vec[_,_<: Nat], Mat[_,_<: Nat,_<: Nat], Size <: Nat : ToInt](x: Mat[T, Size,Size])(implicit ev: AlgebraicSquareMatrix[T,Vec,Mat], space : CanonicalEuclideanSpaceOverField[Vec,T,Size], tensor1: Tensor1[T,Vec,Size], tensor2: Tensor2[T,Mat,Size,Size]) = new SquareMatrixOps[T,Vec,Mat,Size](x)
    implicit def infixMatrixOpsVectorFirst[@tbsp T, Vec[_,_<: Nat], Mat[_,_<: Nat,_<: Nat], Size <: Nat : ToInt](x: Vec[T,Size])(implicit ev: AlgebraicSquareMatrix[T,Vec,Mat], space : CanonicalEuclideanSpaceOverField[Vec,T,Size], tensor1: Tensor1[T,Vec,Size],tensor2: Tensor2[T, Mat, Size, Size]) = new SquareMatrixOpsVectorFirst[T,Vec,Mat,Size](x)
  }


  trait CanonicalEuclideanSpaceOverFieldImplicits{
    implicit def infixCanonicalEuclideanSpaceOps[V[_,_<: Nat],@tbsp F, Dim <: Nat : ToInt](x: V[F, Dim])(implicit num: CanonicalEuclideanSpaceOverField[V,F,Dim]) = new CanonicalEuclideanSpaceOps[V,F, Dim](x)
  }

  trait AlgebraicTypesImplicits{



    implicit def infixVectorOps[V[_,_<: Nat], @tbsp F : ClassTag, Dim <: Nat : ToInt](x: V[F,Dim])(implicit ev: AlgebraicVector[F, V], tensor1: Tensor1[F,V,Dim]) = new StaticVectorOps[F, V, Dim](x)

  }

  trait ExtraImplicits{
    implicit def infixVector2Ops[V[_,_<: Nat], @tbsp F](x: V[F, Nat._2])(implicit ev: TwoDimensionalVectorOrthoOperatorOverCanonicalEuclideanSpaceOverField[V, F]) = new OrthoOps[F,V](x)
    implicit def infixVector3Ops[V[_,_<: Nat], @tbsp F](x: V[F, Nat._3])(implicit ev: CrossProductOverCanonicalEuclideanSpaceOverField[V,F]) = new CrossProductOps[F, V](x)
  }

  trait ConvertibleFromDoubleImplicits{
    implicit class DoubleAs(n:Double) {
      def as[A](implicit ev:ConvertibleFromDouble[A]):A = ev.fromDouble(n)
    }

    implicit def double2ConvertibleFromDouble[A](n: Double)(implicit ev:ConvertibleFromDouble[A]): A = ev.fromDouble(n)
  }

  trait ContainerImplicits{
    implicit def Container1Ops[@tbsp T, Con1](x: Con1)(implicit ev : Container1[T,Con1]): Container1Ops[T,Con1] = new Container1Ops[T,Con1](x)
    implicit def Container2Ops[@tbsp T, Con2](x: Con2)(implicit ev : Container2[T,Con2]): Container2Ops[T,Con2] = new Container2Ops[T,Con2](x)
    implicit def Container3Ops[@tbsp T, Con3](x: Con3)(implicit ev : Container3[T,Con3]): Container3Ops[T,Con3] = new Container3Ops[T,Con3](x)
    implicit def Container4Ops[@tbsp T, Con4](x: Con4)(implicit ev : Container4[T,Con4]): Container4Ops[T,Con4] = new Container4Ops[T,Con4](x)
    implicit def ContainerAnyOps[@tbsp T, Con](x: Con)(implicit ev : ContainerAny[T,Con]): ContainerAnyOps[T,Con] = new ContainerAnyOps[T,Con](x)
  }

  object ContainerImplicits extends ContainerImplicits

  trait AllOps extends AddableOpsImplicits with
  OrderableOpsImplicits with
  CommutativeGroupImplicits with
  FieldImplicits with
  ModuleOverRingImplicits with
  VectorSpaceOverFieldImplicits with
  CanonicalEuclideanSpaceOverFieldImplicits with
  ConvertibleFromDoubleImplicits with
  ContainerImplicits with
  MultiplicativeMonoidImplicits with
  MatrixImplicits with
  AlgebraicTypesImplicits with
  ExtraImplicits with
  TensorImplicits


}

package Russoul.lib.common

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.math.algebra.{Mat, Mat4}

import scala.language.implicitConversions

/**
  * Created by russoul on 13.06.2017.
  */
object Ops {

  class AddableOps[@specialized A](lhs: A)(implicit ev: Addable[A]){
    @inline def +(rhs: A): A = ev.plus(lhs, rhs)

  }

  class OrderableOps[@specialized A](lhs: A)(implicit ev: Orderable[A]){
    @inline def >(rhs: A) : Boolean = ev.gt(lhs, rhs)
    @inline def <(rhs: A) : Boolean = ev.lt(lhs, rhs)
    @inline def >=(rhs: A) : Boolean = ev.gteq(lhs, rhs)
    @inline def <=(rhs: A) : Boolean = ev.lteq(lhs, rhs)
    @inline def =?(rhs: A) : Boolean = ev.equiv(lhs, rhs)
    @inline def ≠?(rhs: A) : Boolean = !ev.equiv(lhs, rhs)
  }



  class CommutativeGroupOps[@specialized A](lhs: A)(implicit ev: CommutativeGroup[A]){
    @inline def -(rhs: A): A = ev.minus(lhs, rhs)
    @inline def unary_-(): A = ev.negate(lhs)
  }

  class FieldOps[@specialized A](lhs: A)(implicit ev: Field[A]){
    @inline def *(rhs: A) : A = ev.times(lhs, rhs)
    @inline def /(rhs: A) : A = ev.div(lhs, rhs)
  }

  class ModuleOps[V,@specialized R](lhs: V)(implicit ev: ModuleOverRing[V,R]){
    @inline def *(rhs: R) : V = ev.times(lhs, rhs)
    @inline def ⊗(rhs: V) : V = ev.timesByElement(lhs, rhs)

    //starting from 1
    @inline def apply(i:Int): R = ev.get(lhs, i)
    @inline def x: R = ev.x(lhs)

    //may be undefined !
    @inline def y: R = ev.y(lhs)
    @inline def z: R = ev.z(lhs)
    @inline def w: R = ev.w(lhs)

    @inline def withWZeroed[VP4](implicit v4: ModuleOverRing[VP4, R]) : VP4  = {
      v4.create(x, y, z, v4.scalar.zero)
    }

    @inline def xyz[VP3](implicit v3: ModuleOverRing[VP3, R]) : VP3  = {
      v3.create(x, y, z)
    }
    //.............


  }


  class VectorSpaceOps[V,@specialized F](lhs: V)(implicit ev: VectorSpaceOverField[V,F]){
    @inline def /(rhs: F) : V = ev.div(lhs, rhs)
  }


  class EuclideanSpaceOps[V,@specialized F](lhs: V)(implicit ev: EuclideanSpaceOverField[V,F]){
    @inline def ⋅(rhs: V)(implicit matGram: Mat[F] with Gram) : F = ev.dotProduct(lhs, rhs, matGram)
    @inline def dot(rhs: V)(implicit matGram: Mat[F] with Gram) : F = ev.dotProduct(lhs, rhs, matGram)
    @inline def normalize()(implicit matGram: Mat[F] with Gram) : V = ev.normalize(lhs, matGram)
    @inline def squaredLength()(implicit matGram: Mat[F] with Gram) : F = ev.squaredLength(lhs, matGram)
  }


  class CanonicalEuclideanSpaceOps[V,@specialized F](lhs: V)(implicit ev: CanonicalEuclideanSpaceOverField[V,F]){
    @inline def ⋅(rhs: V) : F = ev.dotProduct(lhs, rhs)
    @inline def dot(rhs: V) : F = ev.dotProduct(lhs, rhs)
    //can't use * operator because of JVM type erasure (we already have this operator in Field[F] both are erased to Object => Object)
    @inline def normalize() : V = ev.normalize(lhs)
    @inline def squaredLength() : F = ev.squaredLength(lhs)
    @inline def length() : F = ev.scalar.sqrt(ev.squaredLength(lhs))
  }

  class CanonicalCrossProductOps[V](lhs: V)(implicit ev: CanonicalCrossProductOp[V]){
    def ⨯(rhs: V) = ev.crossProduct(lhs, rhs)
  }

  class Canonical2DimCrossProductOps[V](lhs: V)(implicit ev: Canonical2DimOrthoOp[V]){
    def ⟂() = ev.ortho(lhs)
  }

  class Mat4MultOps[V](lhs: V){
    def ⨯[@specialized F](rhs: Mat4[F])(implicit ev: Mat4Mult[V,F]): V = ev.multM(lhs, rhs)
  }



  trait AddableOpsImplicits{
    implicit def infixAddableOps[@specialized A](x: A)(implicit num: Addable[A])= new AddableOps[A](x)
  }

  trait OrderableOpsImplicits{
    implicit def infixOrderableOps[@specialized A](x: A)(implicit num: Orderable[A]) = new OrderableOps[A](x)
  }

  trait CommutativeGroupImplicits{
    implicit def infixCommutativeGroupLikeOps[@specialized A](x: A)(implicit num: CommutativeGroup[A]) = new CommutativeGroupOps[A](x)
  }

  trait FieldImplicits{
    implicit def infixFieldLikeOps[@specialized A](x: A)(implicit num: Field[A]) = new FieldOps[A](x)
  }

  trait ModuleOverRingImplicits{
    implicit def infixModuleOps[V,@specialized R](x: V)(implicit num: ModuleOverRing[V,R]) = new ModuleOps[V,R](x)
  }

  trait VectorSpaceOverFieldImplicits{
    implicit def infixVectorSpaceOps[V,@specialized F](x: V)(implicit num: VectorSpaceOverField[V,F]) = new VectorSpaceOps[V,F](x)
  }

  trait EuclideanSpaceImplicits{
    implicit def infixEuclideanSpaceOps[V,@specialized F](x: V)(implicit num: EuclideanSpaceOverField[V,F]) = new EuclideanSpaceOps[V,F](x)
  }

  trait CanonicalEuclideanSpaceOverFieldImplicits{
    implicit def infixCanonicalEuclideanSpaceOps[V,@specialized F](x: V)(implicit num: CanonicalEuclideanSpaceOverField[V,F]) = new CanonicalEuclideanSpaceOps[V,F](x)
  }

  trait CanonicalCrossProductImplicits{
    implicit def infixCanonicalCrossProductOps[V](x: V)(implicit num: CanonicalCrossProductOp[V]) = new CanonicalCrossProductOps[V](x)
  }

  trait Canonical2DimCrossProductImplicits{
    implicit def infixCanonical2DimOrthoOps[V](x: V)(implicit num: Canonical2DimOrthoOp[V]) = new Canonical2DimCrossProductOps[V](x)
  }

  trait Mat4MultImplicits{
    implicit def infixMat4MultOps[V](x: V): Mat4MultOps[V] = new Mat4MultOps[V](x)
  }

  trait ConvertibleFromDoubleImplicits{
    implicit class DoubleAs(n:Double) {
      def as[A](implicit ev:ConvertibleFromDouble[A]):A = ev.fromDouble(n)
    }

    //TODO make it work !!!
    //implicit def double2ConvertibleFromDouble[A](n: Double)(implicit ev:ConvertibleFromDouble[A]) = ev.fromDouble(n)
  }

  trait AllOps extends AddableOpsImplicits with
  OrderableOpsImplicits with
  CommutativeGroupImplicits with
  FieldImplicits with
  ModuleOverRingImplicits with
  VectorSpaceOverFieldImplicits with
  EuclideanSpaceImplicits with
  CanonicalEuclideanSpaceOverFieldImplicits with
  CanonicalCrossProductImplicits with
  Canonical2DimCrossProductImplicits with
  Mat4MultImplicits with
  ConvertibleFromDoubleImplicits


}

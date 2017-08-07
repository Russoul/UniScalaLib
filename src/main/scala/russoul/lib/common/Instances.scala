package russoul.lib.common

import russoul.lib.common.StaticContainerTypeClasses._
import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra.{Mat, Vec}
import shapeless.Nat
import shapeless.ops.nat.ToInt
import Nat._

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Created by russoul on 13.06.2017.
  */
object Instances {



  object AllInstances extends AllInstances

  trait IntInstances{
    implicit val intIsFullPseudoField = new IntIsFullPseudoField
  }

  trait FloatInstances{
    implicit val floatIsFullField = new FloatIsFullField
  }

  trait DoubleInstances{
    implicit val doubleIsFullField = new DoubleIsFullField
  }

  trait ComplexInstances{
    implicit val complexIsField = new ComplexIsField
  }
  


  trait ArrayInstances{

    /*private val hashTableArrayInstances = new mutable.HashMap[ClassTag[_], ArrayIsContainerAny[_]]()
    implicit def arrayIsContainerAny[@tbsp T](implicit tag : ClassTag[T]): ArrayIsContainerAny[T] = {
      if(hashTableArrayInstances.contains(tag)){
        hashTableArrayInstances(tag).asInstanceOf[ArrayIsContainerAny[T]]
      }else{
        val n = new ArrayIsContainerAny[T]
        hashTableArrayInstances.put(tag, n)
        n
      }
    }*/

    //def ArrayAsEuclideanSpace[@tbsp F : ClassTag](dim: Int)(implicit field: Field[F] with Trig[F] with Euclidean[F]) = new ArrayIsCanonicalEuclideanSpaceOverField[F](dim, field)
  }

  trait DefaultAlgebraicTypeInstances{
    implicit def TIsTensor0[@tbsp T : ClassTag] = new TIsTensor0[T]
    implicit def vecIsTensor1[@tbsp T : ClassTag, A1 <: Nat] = new VecIsTensor1[T,A1]
    implicit def matIsTensor2[@tbsp T : ClassTag, A1 <: Nat, A2 <: Nat] = new MatIsTensor2[T,A1,A2]

    //we get this for free because they are tensors
    implicit def vecIsAlgebraicVector[@tbsp T : ClassTag] = new AlgebraicVector[T, Vec]
    implicit def matIsAlgebraicSquareMatrix[@tbsp T : ClassTag] = new AlgebraicSquareMatrix[T, Vec, Mat]
    //...

    implicit def vecIsModule[@tbsp R : ClassTag, Dim <: Nat](implicit ring: Ring[R], toInt: ToInt[Dim], notField: NoImplicit[RealField[R]]) = new VecIsModule[R,Dim](ring, toInt)
    implicit def vecIsCanEuclideanSpace[@tbsp F : ClassTag, Dim <: Nat](implicit field: RealField[F] with Trig[F] with Euclidean[F], evDim: ToInt[Dim]) = new VecIsCanonicalEuclideanSpaceOverField[F, Dim](field)

    implicit def vec2HasOrtho[@tbsp F](implicit space: CanonicalEuclideanSpaceOverField[Vec,F,_2]) = new Vec2HasOrtho[F](space)
    implicit def vec3HasCrossProduct[@tbsp F](implicit space: CanonicalEuclideanSpaceOverField[Vec,F,_3]) = new Vec3HasCrossProduct[F](space)
  }

  trait SpecialInstances{

  }




  trait AllInstances extends
    IntInstances with
    FloatInstances with
    DoubleInstances with
    ComplexInstances with
    ArrayInstances with
    DefaultAlgebraicTypeInstances
  
  

}

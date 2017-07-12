package Russoul.lib.common

import Russoul.lib.common.StaticContainerTypeClasses.{MatIsStaticMatrix, VecIsStaticVector}
import Russoul.lib.common.TypeClasses._
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Created by russoul on 13.06.2017.
  */
object Instances {



  object AllInstances extends AllInstances

  trait IntInstances{
    implicit val intIsFullRing = new IntIsFullRing
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

    private val hashTableArrayInstances = new mutable.HashMap[ClassTag[_], ArrayIsContainerAny[_]]()
    implicit def arrayIsContainerAny[@specialized T](implicit tag : ClassTag[T]): ArrayIsContainerAny[T] = {
      if(hashTableArrayInstances.contains(tag)){
        hashTableArrayInstances(tag).asInstanceOf[ArrayIsContainerAny[T]]
      }else{
        val n = new ArrayIsContainerAny[T]
        hashTableArrayInstances.put(tag, n)
        n
      }
    }

    //def ArrayAsEuclideanSpace[@specialized F : ClassTag](dim: Int)(implicit field: Field[F] with Trig[F] with Euclidean[F]) = new ArrayIsCanonicalEuclideanSpaceOverField[F](dim, field)
  }

  trait DefaultAlgebraicTypeInstances{
    implicit def vecIsStaticVector[@specialized T : ClassTag] = new VecIsStaticVector[T]
    implicit def matIsStaticMatrix[@specialized T : ClassTag] = new MatIsStaticMatrix[T]

    implicit def vecIsCanEuclideanSpace[@sp F, Dim <: Nat](implicit field: Field[F] with Trig[F] with Euclidean[F], evDim: ToInt[Dim]) = new VecIsCanonicalEuclideanSpaceOverField[F, Dim](field)

    implicit def vec2HasOrtho[@sp F] = new Vec2HasOrtho[F]
    implicit def vec3HasCrossProduct[@sp F] = new Vec3HasCrossProduct[F]
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

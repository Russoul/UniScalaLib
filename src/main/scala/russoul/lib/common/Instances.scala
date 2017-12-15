package russoul.lib.common

import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra.{Mat, Vec}
import shapeless.Nat
import shapeless.ops.nat.ToInt
import Nat._
import spire.algebra.{Field, NRoot, Trig}

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Created by russoul on 13.06.2017.
  */
object Instances {



  object AllInstances extends AllInstances


  trait AlgVectorInstance{
    implicit def algVector[@tbsp F : ClassTag, Size <: Nat](implicit f : Field[F] with Trig[F] with NRoot[F]): AlgVector[F, Size] = new AlgVector[F,Size] {
      override implicit def scalar: Field[F] with Trig[F] with NRoot[F] = f
    }
  }



  trait AllInstances extends AlgVectorInstance

  

}

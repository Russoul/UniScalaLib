package russoul.lib.common

import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra._
import shapeless.{<:!<, Nat}
import shapeless.ops.nat
import shapeless.ops.nat.Diff.Aux
import shapeless.ops.nat.{Diff, GT, Sum}
import shapeless.ops.nat.Sum.Aux

import scala.reflect.ClassTag

/**
  * Created by russoul on 09.07.2017.
  */
object StaticContainerTypeClasses {


  //TODO require it to be a field ?
  class TIsTensor0[@tbsp T : ClassTag] extends Tensor0[T]

  class VecIsTensor1[@tbsp T : ClassTag, A1 <: Nat] extends Tensor1[T, Vec, A1]{
    override def make(args: T*)(implicit ev1: nat.ToInt[A1]): Vec[T, A1] = Vec(args : _*)
    override def get(a: Vec[T, A1], i: Int): T = a(i)
  }

  class MatIsTensor2[@tbsp T : ClassTag, A1 <: Nat, A2 <: Nat] extends Tensor2[T,Mat,A1,A2]{
    override def make(args: T*)(implicit ev1: nat.ToInt[A1], ev2: nat.ToInt[A2]): Mat[T, A1, A2] = Mat[T,A1,A2](args : _*)
    override def get(a: Mat[T, A1, A2], i: Int, j: Int): T = a(i,j)
  }


  

}

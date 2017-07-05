package Russoul.lib.common

import shapeless.Nat
import shapeless.ops.nat.ToInt
import singleton.ops.{==, Require}

import scala.annotation.elidable
import scala.collection.generic.IsTraversableLike
import scala.language.higherKinds

/**
  * Created by russoul on 04.07.2017.
  */
package object typechecker
{

  final val RUNTIME_DIM_CHECK = 10000//for elidable

  class RuntimeDimCheckFailException(a: Int, b: Int) extends Exception("Runtime check of dimension equality failed: " + a + " != " + b)

  @elidable(RUNTIME_DIM_CHECK)
  def runtimeDimCheck(a: Int, b: Int): Unit ={
    if (a != b) {
      throw new RuntimeDimCheckFailException(a, b)
    }
  }

  /*trait NatDim[T]{
    type N
  }
  object NatDim{
    type Aux[T,N0 <: Nat] = NatDim[T] { type N = N0 }
  }


  class StaticSizeIsNatDim[L <: Nat] extends NatDim[StaticSize[L]]{
    override type N = L
  }
  */

  //TODO make it based on implicit conversion or NOT !
  trait StaticSizeCol[T, Repr[_], L <: Nat]{
    def get : Repr[T]
  }

  class Checked[T, Repr[_] <: Traversable[_], L <: Nat](val col: Repr[T])(implicit toInt: ToInt[L]) extends StaticSizeCol[T, Repr, L]{
    runtimeDimCheck(toInt(), col.size)

    override def get = col
  }

  //impl conv to trav
  implicit def checkedToCol[T, Repr[_] <: Traversable[_], L <: Nat](checked: Checked[T, Repr, L]): Repr[T] = checked.get

  implicit class CheckedCollection[T, Repr[_] <: Traversable[_]](col : Repr[T]){
    def checked[N <: Nat](implicit toInt: ToInt[N]) = new Checked[T, Repr, N](col)
  }


  def compileTimeSizeInsurance[T, Repr[_], L <: Nat, N <: Nat](checked: StaticSizeCol[T, Repr, L], nat: N)(implicit req: Require[L == N]): Unit = {}


}

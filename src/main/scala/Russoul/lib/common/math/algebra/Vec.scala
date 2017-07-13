package Russoul.lib.common.math.algebra

import Russoul.lib.common.{immutable, straight}
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

/**
  * Created by russoul on 11.07.2017.
  */
@immutable @straight class Vec[@specialized A : ClassTag, Size <: Nat]private ()(implicit size: ToInt[Size]){
  private val array = new Array[A](size())

  @inline def apply(i: Int): A = array(i - 1)

}

object Vec {
  @inline def apply[@specialized A : ClassTag, Size <: Nat](args: A*)(implicit size: ToInt[Size]): Vec[A, Size] = {
    val result = new Vec[A,Size]()

    var k = 0
    while(k < size()){
      result.array(k) = args(k)
      k += 1
    }

    result
  }
}

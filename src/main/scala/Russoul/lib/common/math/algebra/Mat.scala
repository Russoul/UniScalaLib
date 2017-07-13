package Russoul.lib.common.math.algebra

import Russoul.lib.common.{immutable, sp}
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

/**
  * Created by russoul on 11.07.2017.
  */
@immutable class Mat[@sp T : ClassTag, A1 <: Nat, A2 <: Nat] private()(implicit a1: ToInt[A1], a2: ToInt[A2]){
  private val array = new Array[T](a1() * a2())

  def apply(i: Int, j: Int) = array(i * a2() + j)

}
object Mat{

  def apply[@sp T : ClassTag, Size <: Nat](args: T*)(implicit size: ToInt[Size]) : Mat[T,Size,Size] = {
    val result = new Mat[T,Size,Size]()

    var i = 0

    while(i < size()){
      var j = 0
      while(j < size()){
        result.array(j + i*size()) = args(j + i*size())
        j += 1
      }
      i += 1
    }

    result
  }

  def apply[@sp T : ClassTag, A1 <: Nat, A2 <: Nat](args: T*)(implicit a1: ToInt[A1], a2: ToInt[A2]) : Mat[T,A1,A2] = {
    val result = new Mat[T,A1,A2]()

    var i = 0

    while(i < a1()){
      var j = 0
      while(j < a2()){
        result.array(j + i*a2()) = args(j + i*a2())
        j += 1
      }
      i += 1
    }

    result
  }
}

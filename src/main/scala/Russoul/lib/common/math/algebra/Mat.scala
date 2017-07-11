package Russoul.lib.common.math.algebra

import Russoul.lib.common.{immutable, sp}
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

/**
  * Created by russoul on 11.07.2017.
  */
@immutable class Mat[@sp T : ClassTag, Size <: Nat] private()(implicit size: ToInt[Size]){
  private val array = new Array[T](size() * size())

  def apply(i: Int, j: Int) = array(i + j * size())

}
object Mat{

  def apply[@sp T : ClassTag, Size <: Nat](args: T*)(implicit size: ToInt[Size]) : Mat[T,Size] = {
    val result = new Mat[T,Size]()

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
}

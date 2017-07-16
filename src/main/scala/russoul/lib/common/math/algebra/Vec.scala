package russoul.lib.common.math.algebra

import russoul.lib.common.{immutable, straight, tbsp}
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

/**
  * Created by russoul on 11.07.2017.
  */
@immutable @straight case class Vec[@tbsp A : ClassTag, Size <: Nat]private ()(implicit size: ToInt[Size]){
  private val array = new Array[A](size())

  @inline def apply(i: Int): A = array(i)

  def toArray = array.clone()

}

object Vec {
  @inline def apply[@tbsp A : ClassTag, Size <: Nat](args: A*)(implicit size: ToInt[Size]): Vec[A, Size] = {
    val result = new Vec[A,Size]()

    var k = 0
    while(k < size()){
      result.array(k) = args(k)
      k += 1
    }

    result
  }
}

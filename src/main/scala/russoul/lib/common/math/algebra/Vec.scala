package russoul.lib.common.math.algebra

import russoul.lib.common._
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.collection.TraversableLike
import scala.reflect.ClassTag

/**
  * Created by russoul on 11.07.2017.
  */
@immutable @straight case class Vec[@tbsp A : ClassTag, Size <: Nat]private ()(implicit size: ToInt[Size]) extends Traversable[A]{
  private val array = new Array[A](size())

  @inline def apply(i: Int): A = array(i)

  def toArray = array.clone()

  override def toString() : String = {
    var str = ""

    for(i <- array) str += i + " "

    if(array.size > 1) str = str.dropRight(1)

    s"Vec[${implicitly[ClassTag[A]].toString()}, ${implicitly[ToInt[Size]].apply()}]\n$str"
  }

  override def foreach[U](f: (A) => U): Unit = {
    var k = 0
    while (k < size()){
      f $ array(k)
      k += 1
    }
  }
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

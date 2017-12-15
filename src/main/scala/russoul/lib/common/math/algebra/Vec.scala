package russoul.lib.common.math.algebra

import russoul.lib.common._
import Implicits._
import russoul.lib.macros.Ref
import singleton.ops.XInt

import scala.collection.TraversableLike
import scala.reflect.ClassTag




/**
  * Created by russoul on 11.07.2017.
  */
//size_arg is not a field !
@immutable class Vec[@tbsp A : ClassTag, Size <: XInt]private(size_arg: Int) extends Traversable[A]{

  type E = A
  type N = Size

  private val array = new Array[A](size_arg)

  @inline def apply(i: Int): A = array(i)

  def toArray = array.clone()

  override def toString() : String = {
    var str = ""

    for(i <- array) str += i + " "

    if(array.length > 1) str = str.dropRight(1)

    s"Vec[${implicitly[ClassTag[A]].toString()}, ${array.length}]\n$str"
  }

  override def foreach[U](f: (A) => U): Unit = {
    var k = 0
    while (k < array.length){
      f +> array(k)
      k += 1
    }
  }


  override def hashCode() = {
    array.hashCode()
  }

  override def equals(obj: scala.Any) = {
    obj match {
      case that : Vec[A, Size] => this.hashCode() == that.hashCode()
      case _ => false
    }
  }




}

object Vec {
  @inline def apply[@tbsp A : ClassTag, Size <: XInt](args: A*): Vec[A, Size] = {
    val result = new Vec[A,Size](args.size)

    var k = 0
    while(k < args.size){
      result.array(k) = args(k)
      k += 1
    }


    result
  }

}
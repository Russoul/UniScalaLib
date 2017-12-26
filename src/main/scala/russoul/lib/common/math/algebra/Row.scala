package russoul.lib.common.math.algebra

import russoul.lib.common._
import Implicits._
import russoul.lib.macros.Ref
import singleton.ops.XInt

import scala.collection.TraversableLike
import scala.reflect.ClassTag


/*

/**
  * Created by russoul on 11.07.2017.
  */
//size_arg is not a field !
@immutable class Vec[@tbsp A : ClassTag, Size <: XInt]private(size_arg: Int) extends Traversable[A]{

  type E = A
  type N = Size

  private val array = new Array[A](size_arg)

  override def size() = array.length

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


  override def hashCode(): Int = {
    var code: Int = 0xf457f00d

    for(i <- 0 until size){
      code = (code * 19) + array(i).##
    }

    code
  }


  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that : Vec[A, Size] => this.hashCode() == that.hashCode() && {

        var i = 0
        while(i < size()){
          if(!(this(i) == that(i))) return false
          i += 1
        }
        true
      }
      case _ => false
    }
  }




}*/

object Row {
  @inline def apply[@tbsp A : ClassTag, Size <: XInt](args: A*): Mat[A, _1, Size] = {
    Mat[A, _1, Size](1, args.size, args : _*)

  }

}

object Column {
  @inline def apply[@tbsp A : ClassTag, Size <: XInt](args: A*): Mat[A, Size, _1] = {
    Mat[A, Size, _1](args.size, 1, args : _*)

  }

}

package russoul.lib.common.math.algebra

import russoul.lib.common.{immutable, sp, tbsp}
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

/**
  * Created by russoul on 11.07.2017.
  */
//TODO constructor should be private, but it does not compile this way due to bug with @sp
@immutable class Mat[@tbsp T : ClassTag, A1 <: Nat, A2 <: Nat] ()(implicit a1: ToInt[A1], a2: ToInt[A2]){


  private val array = new Array[T](a1() * a2())

  def apply(i: Int, j: Int) = array(i * a2() + j)

  def toArray = array.clone()

  override def toString : String = { //TODO probably use stringbuilder for better performance
    var str = ""

    for(i <- 0 until a1()){
      for(j <- 0 until a2()){
        str += this(i,j) + " "
      }
      str.dropRight(1)
      str += "\n"
    }

    s"Mat[${implicitly[ClassTag[T]].toString()}, ${a1()}, ${a2()}]\n$str"
  }

  override def hashCode() = {
    array.hashCode()
  }

  override def equals(obj: scala.Any) = {
    obj match {
      case that : Mat[T, A1,A2] => this.hashCode() == that.hashCode()
      case _ => false
    }
  }

}


object Mat{

  def apply[@tbsp T : ClassTag, Size <: Nat](args: T*)(implicit size: ToInt[Size]) : Mat[T,Size,Size] = {
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



  def apply[@tbsp T : ClassTag, A1 <: Nat, A2 <: Nat](args: T*)(implicit a1: ToInt[A1], a2: ToInt[A2]) : Mat[T,A1,A2] = {
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

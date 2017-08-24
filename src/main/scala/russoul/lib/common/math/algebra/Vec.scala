package russoul.lib.common.math.algebra

import russoul.lib.common.TypeClasses.Addable
import russoul.lib.common._
import shapeless.{Nat, _0}
import shapeless.ops.nat.{Diff, ToInt}
import shapeless.ops.nat._
import shapeless.ops.nat.Diff._
import Implicits._

import scala.collection.TraversableLike
import scala.reflect.ClassTag

sealed trait FVec[+A,N <: Nat]
object FNil extends FVec[Nothing, _0] //TODO this implementation takes more space than Array impl + more `new` calls
case class Cons[+A,Q <: Nat,N <: Nat](x : A, xs : FVec[A, Q])(implicit diff : Sum.Aux[Q,Nat._1,N]) extends FVec[A,N]
//TODO problem : each node of FVec now contains extra implicit value as field !
//solution : move implicit from class constructor to object.apply

object FVec{

  val test : FVec[Int, Nat._3] = Cons (3, Cons (2, Cons(1, FNil)))
}

/**
  * Created by russoul on 11.07.2017.
  */
@immutable class Vec[@tbsp A : ClassTag, Size <: Nat]private ()(implicit size: ToInt[Size]) extends Traversable[A]{
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

package russoul.lib.common.math.algebra

import russoul.lib.common._
import russoul.lib.macros.Ref
import singleton.ops.{Id, XInt}
import spire.algebra.{Field, VectorSpace}

import scala.collection.TraversableLike
import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.implicits._
import singleton.ops._
import spire.{NoImplicit, sp}

/**
  * Created by russoul on 11.07.2017.
  */
//size_arg is not a field !
class Vec[@specialized(Int, Float, Double, Long) A : ClassTag, Size <: XInt]private(size_arg: Int) extends Traversable[A]{

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
      f(array(k))
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
  @inline def apply[@specialized(Int, Float, Double, Long) A : ClassTag, Size <: XInt](args: A*): Vec[A, Size] = {
    val result = new Vec[A,Size](args.size)

    var k = 0
    while(k < args.size){
      result.array(k) = args(k)
      k += 1
    }


    result
  }

}

final class VecIsModule[@specialized(Float, Double, Int, Long) F : ClassTag : Ring, Size <: XInt](implicit _size : ValueOf[Size]) extends Module [Vec[F,Size],F] {
  override def scalar: Ring[F] = Ring[F]

  override def timesl(r: F, v: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](v.size)

    var i = 0
    while (i < ar.size){
      ar(i) = r * v(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def negate(v: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](v.size)

    var i = 0
    while (i < ar.size){
      ar(i) = -v(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def zero: Vec[F, Size] = {
    val ar = new Array[F](_size.value)

    var i = 0
    while (i < ar.size){
      ar(i) = scalar.zero
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def plus(a: Vec[F, Size], b: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](a.size)

    var i = 0
    while (i < a.size){
      ar(i) = a(i) + b(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }
}

final class VecIsVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends VectorSpace [Vec[F,Size],F]{
  override def scalar: Field[F] = Field[F]

  override def timesl(r: F, v: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](v.size)

    var i = 0
    while (i < ar.size){
      ar(i) = r * v(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def negate(v: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](v.size)

    var i = 0
    while (i < ar.size){
      ar(i) = -v(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def zero: Vec[F, Size] = {
    val ar = new Array[F](_size.value)

    var i = 0
    while (i < ar.size){
      ar(i) = scalar.zero
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def plus(a: Vec[F, Size], b: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](a.size)

    var i = 0
    while (i < a.size){
      ar(i) = a(i) + b(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }
}




final class VecIsNormedVectorSpace[@specialized(Float, Double) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size], nroot : NRoot[F]) extends NormedVectorSpace [Vec[F,Size],F]{
  override def scalar: Field[F] = Field[F]

  override def timesl(r: F, v: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](v.size)

    var i = 0
    while (i < ar.size){
      ar(i) = r * v(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def negate(v: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](v.size)

    var i = 0
    while (i < ar.size){
      ar(i) = -v(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def zero: Vec[F, Size] = {
    val ar = new Array[F](_size.value)

    var i = 0
    while (i < ar.size){
      ar(i) = scalar.zero
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def plus(a: Vec[F, Size], b: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](a.size)

    var i = 0
    while (i < a.size){
      ar(i) = a(i) + b(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def norm(v: Vec[F, Size]): F = {
    var ret = scalar.zero
    var i = 0
    while (i < v.size){
      ret += v(i)
      i += 1
    }

    ret.sqrt()
  }
}


final class VecIsInnerProductSpace[@specialized(Double, Float) F : ClassTag : Field, Size <: XInt](implicit _size : ValueOf[Size]) extends InnerProductSpace [Vec[F,Size],F]{
  override def scalar: Field[F] = Field[F]

  override def timesl(r: F, v: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](v.size)

    var i = 0
    while (i < ar.size){
      ar(i) = r * v(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def negate(v: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](v.size)

    var i = 0
    while (i < ar.size){
      ar(i) = -v(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def zero: Vec[F, Size] = {
    val ar = new Array[F](_size.value)

    var i = 0
    while (i < ar.size){
      ar(i) = scalar.zero
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def plus(a: Vec[F, Size], b: Vec[F, Size]): Vec[F, Size] = {
    val ar = new Array[F](a.size)

    var i = 0
    while (i < a.size){
      ar(i) = a(i) + b(i)
      i += 1
    }

    Vec[F, Size](ar : _*)
  }

  override def dot(a: Vec[F, Size], b: Vec[F, Size]): F = {
    var i = 0
    var r = scalar.zero
    while (i < a.size){
      r += a(i) * b(i)
      i += 1
    }

    r
  }


}

trait VecInstanceM1 {
  type NI0[F,S <: XInt] = NoImplicit[VectorSpace[Vec[F,S], F]]

  implicit def vecModule[@sp(Int,Long,Float,Double) F: ClassTag: Ring, Size <: XInt : ValueOf](implicit no : NI0[F,Size]): VecIsModule[F, Size] =
    new VecIsModule[F,Size]
}

trait VecInstance0 extends VecInstanceM1{
  type NI1[F,S <: XInt] = NoImplicit[InnerProductSpace[Vec[F,S], F]]

  implicit def vecIsVectorSpace[@sp(Float, Double) F : ClassTag : Field, Size <: XInt : ValueOf](implicit no : NI1[F,Size]) : VecIsVectorSpace[F,Size] = {
    new VecIsVectorSpace[F, Size]()
  }
}

trait VecInstance1 extends VecInstance0{
  implicit def vecIsInnerProductSpace[@sp(Float, Double) F : ClassTag : Field, Size <: XInt : ValueOf] : VecIsInnerProductSpace[F,Size] = {
    new VecIsInnerProductSpace[F, Size]()
  }
}


trait VecInstance2 extends VecInstance1{
  implicit def vecIsNormedVectorSpace[@sp(Float, Double) F : ClassTag : Field : NRoot, Size <: XInt : ValueOf] : VecIsNormedVectorSpace[F,Size] = {
    new VecIsNormedVectorSpace[F, Size]()
  }
}

trait VecInstances extends VecInstance2

object VecInstances extends VecInstances
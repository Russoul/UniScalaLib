package russoul.lib.common

import algebra.Order
import russoul.lib.common.math.algebra.{Vec, VecIsNormedVectorSpace}
import russoul.lib.common.math.algebra.VecInstances._
import russoul.lib.macros.{Enricher, Ref, ref, row}
import singleton.ops._
import spire.algebra._
import spire.math._
import spire.implicits._
import singleton.ops._

import scala.reflect.ClassTag // provides infix operators, instances and conversions



//noinspection SyntaxError
object Test extends App{

  type _1 = 1
  type _2 = 2
  type _3 = 3


  def verifyInput[T](i : Int, ar : Array[T]) : Boolean = {
    i < ar.length
  }

  @ref def sum2[T : Ring : ClassTag](a : Array[T], b : Array[T])
                                    (implicit _r : Ref["a.length == b.length && a.length == 2 | true"]): Array[T] = {
    Array[T](a(0) + b(0), a(1) + b(1))
  }

  val ret = sum2(Array(1,2), Array(-1,-2))
  println(ret(0))


  def demo[L <: XInt](implicit p : Id[L]) : p.Out {} = {
    p.value
  }
  val bb : Int = demo[5]


  class VectorSpaceOps[V](lhs : V){
    //no sp needed
    //def :/[F](rhs:F)(implicit ev: VectorSpace[V, F]): V = macro Ops.binopWithEv[F, VectorSpace[V, F], V]
    def *[F](rhs:F)(implicit ev: VectorSpace[V, F]): V = macro Enricher.binopWithEv_timesr[F, VectorSpace[V, F], V]
  }

  implicit def vectorSpaceOps[V](lhs : V) : VectorSpaceOps[V] = new VectorSpaceOps[V](lhs)

  val v1 = Vec[Float,_2](1F,2F)
  val v2 = Vec[Float,_2](1F,2F)
  val v3 = v1 + v1

  val v4 = v1 * 2F

  val a = Vec[Float, _3](1F,2F,3F)
  val b = row(-1F,-2F,-3F)

  val c = a + (b - a) * implicitly[Field[Float]].fromDouble(0.5D)
  println(c)


  a.normalize

  implicitly[VectorSpace[Vec[Float,_2],Float]]

}

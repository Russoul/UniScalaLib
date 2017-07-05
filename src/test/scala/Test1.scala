import MainTest._
import Russoul.lib.common.TypeClasses
import shapeless.Nat
import shapeless.ops.nat.ToInt
import singleton.ops.{==, Require}
import Russoul.lib.common.typechecker._
import spire.sp

import scala.language.higherKinds

/**
  * Created by russoul on 04.07.2017.
  */
object Test1 {

  import spire.algebra._   // provides algebraic type classes
  import spire.math._      // provides functions, types, and type classes
  import spire.implicits._ // provides infix operators, instances and conversions
  import Russoul.lib.common.Implicits._

  case class Triangle[Repr <: StaticSizeCol[F,V,Nat._3], V[_], @sp(Int, Long, Float, Double) F : Field : NRoot](v1: Repr, v2: Repr, v3: Repr)(implicit c: (Repr => V[F]), inner: InnerProductSpace[V[F],F], normed : NormedVectorSpace[V[F],F]) {}
  case class TriangleSimple[V[_], F : TypeClasses.Field](v1: V[F], v2: V[F], v3: V[F]) {}

  trait TypeClass[T]

  class DoubleIsTypeClass extends TypeClass[Double]
  implicit val forDouble = new DoubleIsTypeClass

  case class User[V[_], T : TypeClass](a: V[T])

  def test(): Unit =
  {
    TriangleSimple(List(1D,2D,3D), List(1D,2D,3D), List(1D,2D,3D))

    val ar = List(1D,2D,3D)
    val checked = ar.checked[Nat._3]

    Triangle[Checked[Double, List, Nat._3], List, Double](checked,checked,checked)
  }

  def test1(): Unit =
  {
    val ar = List(1D,2D,3D)
    val checked = ar.checked[Nat._3]

    //Triangle[checked.type, List, Float](checked, checked, checked)
    //

  }
}

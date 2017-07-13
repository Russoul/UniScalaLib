import Russoul.lib.common.{TypeClasses, immutable}
import Russoul.lib.common.math.geometry.simple.general.Shape3
import shapeless.Nat
import russoul.lib.typechecker._
import russoul.lib.macros._

import scala.language.higherKinds

/**
  * Created by russoul on 04.07.2017.
  */
object Test1 {

 /* import spire.algebra._   // provides algebraic type classes
  import spire.implicits._ // provides infix operators, instances and conversions

  case class TriangleSimple[V[_], F : TypeClasses.Field](v1: V[F], v2: V[F], v3: V[F]) {}

  trait TypeClass[T]

  class DoubleIsTypeClass extends TypeClass[Double]
  implicit val forDouble = new DoubleIsTypeClass

  case class User[V[_], T : TypeClass](a: V[T])


  trait NormedInnerProductSpace[V, @sp(Float, Double) F] extends Any with NormedVectorSpace[V, F] {
    def space: InnerProductSpace[V, F]
    def scalar: Field[F] = space.scalar
    def nroot: NRoot[F]

    def zero: V = space.zero
    def plus(v: V, w: V): V = space.plus(v, w)
    def negate(v: V): V = space.negate(v)
    override def minus(v: V, w: V): V = space.minus(v, w)
    def timesl(f: F, v: V): V = space.timesl(f, v)
    override def divr(v: V, f: F): V = space.divr(v, f)
    def norm(v: V): F = nroot.sqrt(space.dot(v, v))
  }


  implicit def innerProductSpaceToNormedInner[V,@specialized F](innerProductSpace: InnerProductSpace[V,F])(implicit ev: NRoot[F]) = new NormedInnerProductSpace[V, F] {
    def space = innerProductSpace
    def nroot: NRoot[F] = ev
  }

  @immutable case class TriangleOver[V[_], @sp(Int, Long, Float, Double) F : Field : NRoot](p1:StaticSizeCol[F,V,Nat._3], p2:StaticSizeCol[F,V,Nat._3], p3:StaticSizeCol[F,V,Nat._3])(implicit view: V[F] => Traversable[F], space: NormedVectorSpace[V[F],F]) extends Shape3[V[F],F] {
    type Repr = StaticSizeCol[F,V,Nat._3]

    override def translate(v: V[F]): TriangleOver[V,F] = {
      TriangleOver[V, F](makeChecked[V,F,Nat._3,No](p1.get + v), makeChecked[V,F,Nat._3,No](p2.get + v), makeChecked[V,F,Nat._3,No](p3.get + v))
    }

    override def toString: String = {
      "Triangle(point1 = " + p1 + ";point2 = " + p2 + ";point3 = " + p3 + " )"
    }
  }


  def test(): Unit =
  {


  }




  def test1(): Unit =
  {
    val ar = List(1D,2D,3D)
    val checked = makeChecked[List, Double, Nat._3, Yes](ar)

    val tr = TriangleOver[List, Double](checked, checked, checked)
  }*/
}

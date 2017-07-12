import Russoul.lib.common.TypeClasses.{CanonicalEuclideanSpaceOverField, StaticVector}
import shapeless._
import shapeless.ops.nat

import scala.reflect.ClassTag
import Russoul.lib.common.Implicits._
import Russoul.lib.common.math.algebra.Vec

/**
  * Created by russoul on 03.07.2017.
  */
object Test extends App{

  /*case class Vec2[@specialized A : ClassTag](array: Array[A]) extends StaticVector[A, Vec2[A], Nat._2] {

    def x = array(0)
    def y = array(1)

    override def get[Index <: Nat](index: Index)(implicit ev: <:!<[_root_.shapeless.Nat._2, Index], toInt: nat.ToInt[Index]): A = {
      array(toInt())
    }
  }
  object Vec2{
    @inline def apply[@specialized A : ClassTag](x: A, y: A): Vec2[A] = Vec2[A](Array[A](x,y))
  }

  trait Addable[@specialized A] {
    def plus(x: A, y: A): A
  }

  class IntIsAddable extends Addable[Int]{
    override def plus(x: Int, y: Int): Int = x + y
  }

  class Vec2IsAddable[@specialized A : ClassTag](implicit ev: Addable[A]) extends Addable[Vec2[A]]{
    override def plus(a: Vec2[A], b: Vec2[A]): Vec2[A] = {
      Vec2(ev.plus(a.x, b.x), ev.plus(a.y, b.y))
    }
  }

  implicit val forInt = new IntIsAddable
  implicit val forVecInt = new Vec2IsAddable[Int]

  def add[@specialized A : Addable](a: A, b: A): A = {
    implicitly[Addable[A]].plus(a, b)
  }

  add(Vec2(1,2), Vec2(2,3))

  val v = Vec2(1,2)
  v.get(Nat._0)*/


  val t = implicitly[CanonicalEuclideanSpaceOverField[Vec,Float,Nat._5]]
}

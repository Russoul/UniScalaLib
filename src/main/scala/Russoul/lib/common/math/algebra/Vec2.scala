package Russoul.lib.common.math.algebra

import Russoul.lib.common.TypeClasses.StaticVector
import Russoul.lib.common.utils.ImArr
import Russoul.lib.common.{immutable, mutable, straight}
import shapeless.Nat
import shapeless.ops.nat

import scala.collection.immutable
import scala.reflect.ClassTag


@immutable @straight case class Vec2[@specialized A : ClassTag](private val array:Array[A]){

  @inline def x = array(0)
  @inline def y = array(1)

  def this(dx:A,dy:A) = this(Array[A](dx,dy))
  @inline def apply(i: Int): A = array(i - 1)
  override def toString() = "Vec2( " + array(0) + "; " + array(1) + " )"


}

object Vec2 {
  @inline def apply[@specialized A : ClassTag](x: A, y: A) = new Vec2[A](x,y)
}

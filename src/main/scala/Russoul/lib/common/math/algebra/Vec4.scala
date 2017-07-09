package Russoul.lib.common.math.algebra

import Russoul.lib.common.utils.ImArr
import Russoul.lib.common.{immutable, straight}

import scala.collection.immutable
import scala.reflect.ClassTag
/**
  *
  *
  * immutable
  */
@immutable @straight case class Vec4[@specialized A : ClassTag](private val array:ImArr[A]) {

  @inline def x: A = array(0)

  @inline def y: A = array(1)

  @inline def z: A = array(2)

  @inline def w: A = array(3)

  def this(dx:A, dy:A, dz:A, dw:A)
  {
    this(ImArr[A](dx,dy,dz,dw))
  }


  /**
    *
    * @param index - starts from 1 !
    * @return
    */
  @inline def apply(index: Int): A = {
    array(index-1)
  }


  override def toString(): String = {
    "Vec4( " + x + "; " + y + "; " + z + "; " + w + " )"
  }


}

object Vec4 {
  @inline def apply[@specialized A : ClassTag](x: A, y: A, z: A, w: A) = new Vec4[A](x,y,z,w)
  @inline def apply[@specialized A : ClassTag](v:Vec3[A], w:A) = new Vec4[A](v.x, v.y, v.z, w)
}


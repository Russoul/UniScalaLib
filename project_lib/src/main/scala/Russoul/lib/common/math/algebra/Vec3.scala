package Russoul.lib.common.math.algebra

import Russoul.lib.common.{Real, immutable, straight}

import scala.reflect.ClassTag


/**
  *
  * immutable
  */



@immutable @straight case class Vec3[@specialized A : ClassTag](array:Array[A]) {

  @inline def x: A = array(0)

  @inline def y: A = array(1)

  @inline def z: A = array(2)


  def this(dx:A,dy:A,dz:A){
    this(Array[A](dx,dy,dz))
  }



  /**
    *
    * @param i - starts from 1 !
    * @return
    */
  @inline def apply(i: Int): A = {
    array(i-1)
  }


  override def toString(): String = {
    "Vec3( " + x + "; " + y + "; " + z + " )"
  }


  /*def <(right:Vec3[A]):Boolean = { //TODO probably not needed
    if (ev.lt(x , right.x))
      return true
    else if (ev.gt(x , right.x))
      return false

    if (ev.lt(y , right.y))
      return true
    else if (ev.gt(y , right.y))
      return false

    if (ev.lt(z , right.z))
      return true
    else if (ev.gt(z , right.z))
      return false

    false
  }*/
}



object Vec3 {
  @inline def apply[@specialized A : ClassTag](x: A, y: A, z: A) = new Vec3[A](x,y,z)
  @inline def apply[@specialized A : ClassTag](v2:Vec2[A], z:A)  = new Vec3[A](v2.x, v2.y, z)
}

/*case class Vec3OverReal(override val array : Array[Real]) extends Vec3[Real](array){

}*/


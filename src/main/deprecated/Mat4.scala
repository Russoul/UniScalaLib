package Russoul.lib.common.math.algebra

import Russoul.lib.common.{immutable, sp}

import scala.reflect.ClassTag

/**
  * Created by russoul on 10.07.2017.
  */
@immutable case class Mat4[@tbsp T : ClassTag] (private val array: Array[T]){

  def apply(i: Int, j: Int) = array(i + j * Mat4.DIM)

}
object Mat4{

  private final val DIM = 4

  def apply[@tbsp T : ClassTag](args: T*) : Mat4[T] = {
    Mat4[T](args.toArray[T])
  }

}

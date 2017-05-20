import Russoul.lib.macros.VecMeta

import scala.reflect.ClassTag
import Russoul.lib.common.math.immutable.algebra._

/**
  * Created by russoul on 01.04.17.
  */

import scala.language.implicitConversions

/**
  * Created by russoul on 18.05.17.
  */



object Test extends App
{



  def add[B](a: Vec[Two,B], b: Vec[Two ,B]): Unit = {

  }


  val vec1 = Vec(2.dim, 1F,2F)
  val vec2 = Vec(2.dim, 0F,3F)
  val vec3 = Vec(Three, 0F,3F)
  val vec4 = Vec(Dim(4), 1F,2F,3F,4F)

  val dotted = vec1 * vec2

  println(dotted)
  println(vec1)
  println(vec4)


}

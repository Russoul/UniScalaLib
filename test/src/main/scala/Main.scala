import russoul.lib.common.math.algebra.Vec1
import russoul.lib.macros.vec
import shapeless.Nat

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object Main extends App{
  val t = vec(1F,2F,3F,4F,5F)
  println(t)

  val t1 = Vec1[Float, Nat._3](1F,2F,3F)

  val tag = implicitly[ClassTag[t1.N]]

  println(tag)
}

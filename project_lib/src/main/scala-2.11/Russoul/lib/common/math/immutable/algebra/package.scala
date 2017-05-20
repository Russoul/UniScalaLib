package Russoul.lib.common.math.immutable

/**
  * Created by russoul on 20.05.17.
  */
package object algebra {
  case class Dim(n:Int)

  object Two extends Dim(2)
  object Three extends Dim(3)

  type Two = Two.type
  type Three = Three.type

  implicit class Int2Dim(i:Int){
    def dim: Dim = Dim(i)
  }
}

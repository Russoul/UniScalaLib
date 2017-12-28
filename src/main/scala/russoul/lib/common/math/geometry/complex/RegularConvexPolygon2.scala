package russoul.lib.common.math.geometry.complex

import russoul.lib.common.Vec2
import singleton.ops._

/**
  * Created by russoul on 15.07.2017.
  */
case class RegularConvexPolygon2[@specialized(Specializable.Bits32AndUp) T, N <: XInt : ValueOf](val center: Vec2[T], val rad: T, val sideCount: N) {
  def getN() = sideCount
}

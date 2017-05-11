package Russoul.lib.common

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import Russoul.lib.common.math.immutable.algebra.Interval
import Russoul.lib.common.math.{CollisionEngine, Math}
import Russoul.lib.common.math.immutable.geometry.simple.{AABB, Circle, Line2, Rectangle2}
import Russoul.lib.common.math.immutable.linear.{vec2, vec3}
import Russoul.lib.common.utils.{ImageUtils, vector}
/**
  * Created by russoul on 01.04.17.
  */
object Test extends App
{

  val t = vec2(1,1)

  val x = t.x

  println(x)

  val aabb = AABB(vec3(0,0,0), vec3(1,1,1))

  println(aabb)

}

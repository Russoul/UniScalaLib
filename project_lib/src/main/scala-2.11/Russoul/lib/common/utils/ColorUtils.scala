package Russoul.lib.common.utils

import Russoul.lib.common.math.immutable.linear.Vec3


object ColorUtils
{

  implicit class SmartVec3ToRGB(rgb:Int){
    def genRGB(): Vec3[Float] =
    {
      val t = 0xFF

      val r = (rgb >> 16) & t
      val g = (rgb >> 8) & t
      val b = rgb & t

      Vec3(r / 255F, g / 255F, b / 255F)
    }
  }

  def genRGB(color: Vec3[Float]): Int =
  {
    genRGB(color.x, color.y, color.z)
  }

  def genRGB(r: Float, g: Float, b: Float): Int =
  {
    val ir = (r * 255).toInt
    val ig = (g * 255).toInt
    val ib = (b * 255).toInt

    genRGB(ir, ig, ib)
  }

  def genRGB(ir: Int, ig: Int, ib: Int): Int =
  {
    var re = ib
    re |= (ig << 8)
    re |= (ir << 16)

    re
  }

  def genRGB(rgb: Int): Vec3[Float] =
  {
    val t = 0xFF

    val r = (rgb >> 16) & t
    val g = (rgb >> 8) & t
    val b = rgb & t

    Vec3(r / 255F, g / 255F, b / 255F)
  }


}

package russoul.lib.common.utils

import russoul.lib.common.{Float3, Vec3}
import russoul.lib.common.TypeClasses.{Euclidean, Field, Orderable}
import russoul.lib.common.Implicits._


object ColorUtils
{
  //TODO colors start from capital letter ?
  final val Red = Float3(1F,0F,0F)
  final val Green = Float3(0F,1F,0F)
  final val Blue = Float3(0F,0F,1F)
  final val Yellow = Float3(1F,1F,0F)
  final val Magenta = Float3(1F,0F,1F)
  final val Cyan = Float3(0F,1F,1F)
  final val Black = Float3(0F,0F,0F)
  final val White = Float3(1F,1F,1F)



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
    genRGB(color(0), color(1), color(2))
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

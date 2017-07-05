package Russoul.lib.common.math.geometry.simple.general


/**
  * Created by russoul on 11.05.17.
  */
trait Shape3[V,F]
{
  def translate(v:V): Shape3[V,F]
}

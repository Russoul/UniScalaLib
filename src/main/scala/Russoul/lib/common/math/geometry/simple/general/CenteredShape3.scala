package Russoul.lib.common.math.geometry.simple.general


/**
  * Created by russoul on 11.05.17.
  */
trait CenteredShape3[V,F] extends Shape3[V,F]
{
  def center:V

  /**
    *
    * @param factor
    * @return scaled around its center version
    */
  def scale(factor:F):CenteredShape3[V,F]
}

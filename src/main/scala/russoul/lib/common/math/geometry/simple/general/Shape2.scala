package russoul.lib.common.math.geometry.simple.general


/**
  * Created by russoul on 11.05.17.
  */
trait Shape2[V,F] {
  def translate(v:V): Shape2[V,F]
  def scaleAroundBasis(factor:F):Shape2[V,F]
}

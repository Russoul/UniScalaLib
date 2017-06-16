package Russoul.lib.common.math.geometry.complex

import Russoul.lib.common.TypeClasses.{CrossProduct, Gram}
import Russoul.lib.common.math.algebra.{Mat, Vec3}
import Russoul.lib.common.{Real, Real3}
import Russoul.lib.common.math.geometry.simple.{PlaneOver, RectangleOver, SphereOver}
import Russoul.lib.common.utils.Arr
import Russoul.lib.common.Implicits._


class Frustum(val pos: Vec3[Real], val angleOfView: Real, val aspect: Real, val lookingDir: Vec3[Real], val zNear: Real, val zFar: Real, val up: Vec3[Real])
{

  def genSphericalBound(near: RectangleOver[Real3,Real], far: RectangleOver[Real3, Real]): SphereOver[Real3, Real] =
  {
    val centerf = (zFar - zNear) / 2
    val n = -far.genNormal()
    val pc = near.center
    val sc = pc + (n * centerf) //sphere center

    SphereOver(sc, centerf)

  }


  /**
    *
    * @return facing the camera, out of the frustum
    */
  def genNearPlane(): RectangleOver[Real3, Real] =
  {
    val center = pos + lookingDir * zNear

    val tan = math.tan(math.toRadians(angleOfView / 2)).toFloat

    val ext1 = tan * zNear //width/2

    val v1 = lookingDir ⨯ up

    val ext1v = v1 * (ext1 * aspect)
    val ext2v = up * ext1

    val nearPlane:RectangleOver[Real3, Real] = new RectangleOver(center, ext1v, ext2v)

    nearPlane
  }

  /**
    *
    * @return facing the camera, out of the frustum
    */
  def genFarPlane(): RectangleOver[Real3, Real] =
  {
    val center = pos + lookingDir * zFar

    val tan = math.tan(math.toRadians(angleOfView / 2)).toFloat

    val ext1 = tan * zFar //width/2

    val v1 = lookingDir.⨯(up)

    val ext1v = v1 * (ext1 * aspect)
    val ext2v = up * ext1

    val farPlane = new RectangleOver[Real3, Real](center, ext1v, ext2v)

    farPlane
  }

  /**
    * 0 - left, 1 - right, 2 - top, 3 - bottom
    *
    * @return normals face into the frustum, list of planes, point and normal for each
    */
  def genExtraPlanes(): Arr[PlaneOver[Real3, Real]] =
  {
    val n = genNearPlane()
    val f = genFarPlane()

    val nvs = n.genVertices()
    val fvs = f.genVertices()
    val nlb = nvs(0)
    val nlt = nvs(3)
    val flb = fvs(0)

    val lb = flb - nlb
    val nl = nlt - nlb

    val normalLeft = lb.⨯(nl).normalize()

    val nrb = nvs(1)
    val nrt = nvs(2)
    val frb = fvs(1)

    val rb = nrb - frb
    val nr = nrt - nrb

    val normalRight = rb.⨯(nr).normalize()


    val frt = fvs(2)

    val nt = nlt - nrt
    val tr = frt - nrt

    val normalTop = nt.⨯(tr).normalize()


    val nb = nrb - nlb

    val normalBottom = nb.⨯(-rb).normalize()


    Arr[PlaneOver[Real3, Real]](PlaneOver[Real3, Real](nlb, normalLeft), PlaneOver[Real3, Real](nrb, normalRight), PlaneOver[Real3, Real](frt, normalTop), PlaneOver[Real3, Real](flb, normalBottom))
  }
}


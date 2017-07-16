package russoul.lib.common.math.geometry.complex

import russoul.lib.common.{Real, Real3, RealF, Vec3}
import russoul.lib.common.math.geometry.simple.{PlaneOver, RectangleOver, SphereOver}
import russoul.lib.common.utils.Arr
import russoul.lib.common.Implicits._
import russoul.lib.common.math.algebra.Vec


case class Frustum(val pos: Vec3[RealF], val angleOfView: RealF, val aspect: RealF, val lookingDir: Vec3[RealF], val zNear: RealF, val zFar: RealF, val up: Vec3[RealF])
{

  def genSphericalBound(near: RectangleOver[Vec,RealF], far: RectangleOver[Vec, RealF]): SphereOver[Vec, RealF] =
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
  def genNearPlane(): RectangleOver[Vec, RealF] =
  {
    val center = pos + lookingDir * zNear

    val tan = math.tan(math.toRadians(angleOfView / 2)).toFloat

    val ext1 = tan * zNear //width/2

    val v1 = lookingDir ⨯ up

    val ext1v = v1 * (ext1 * aspect)
    val ext2v = up * ext1

    val nearPlane = RectangleOver(center, ext1v, ext2v)

    nearPlane
  }

  /**
    *
    * @return facing the camera, out of the frustum
    */
  def genFarPlane(): RectangleOver[Vec, RealF] =
  {
    val center = pos + lookingDir * zFar

    val tan = math.tan(math.toRadians(angleOfView / 2)).toFloat

    val ext1 = tan * zFar //width/2

    val v1 = lookingDir.⨯(up)

    val ext1v = v1 * (ext1 * aspect)
    val ext2v = up * ext1

    val farPlane = RectangleOver(center, ext1v, ext2v)

    farPlane
  }

  /**
    * 0 - left, 1 - right, 2 - top, 3 - bottom
    *
    * @return normals face into the frustum, list of planes, point and normal for each
    */
  def genExtraPlanes(): Array[PlaneOver[Vec, RealF]] =
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


    Array[PlaneOver[Vec, RealF]](PlaneOver(nlb, normalLeft), PlaneOver(nrb, normalRight), PlaneOver(frt, normalTop), PlaneOver(flb, normalBottom))
  }
}


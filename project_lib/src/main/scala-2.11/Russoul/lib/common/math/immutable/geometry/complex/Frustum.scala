package Russoul.lib.common.math.immutable.geometry.complex

import Russoul.lib.common.math.immutable.geometry.simple.{Plane, Rectangle, Sphere}
import Russoul.lib.common.math.immutable.linear.vec3
import Russoul.lib.common.utils.vector


class Frustum(val pos: vec3,val angleOfView: Float,val aspect: Float,val lookingDir: vec3,val zNear: Float,val zFar: Float,val up: vec3)
{

  def genSphericalBound(near: Rectangle, far: Rectangle): Sphere =
  {
    val centerf = (zFar - zNear) / 2
    val n = -far.genNormal()
    val pc = near.center
    val sc = pc + (n * centerf) //sphere center

    Sphere(sc, centerf)

  }


  /**
    *
    * @return facing the camera, out of the frustum
    */
  def genNearPlane(): Rectangle =
  {
    val center = pos + lookingDir * zNear

    val tan = math.tan(math.toRadians(angleOfView / 2)).toFloat

    val ext1 = tan * zNear //width/2

    val v1 = lookingDir.crossProduct(up)

    val ext1v = v1 * (ext1 * aspect)
    val ext2v = up * ext1

    val nearPlane = new Rectangle(center, ext1v, ext2v)

    nearPlane
  }

  /**
    *
    * @return facing the camera, out of the frustum
    */
  def genFarPlane(): Rectangle =
  {
    val center = pos + lookingDir * zFar

    val tan = math.tan(math.toRadians(angleOfView / 2)).toFloat

    val ext1 = tan * zFar //width/2

    val v1 = lookingDir.crossProduct(up)

    val ext1v = v1 * (ext1 * aspect)
    val ext2v = up * ext1

    val farPlane = new Rectangle(center, ext1v, ext2v)

    farPlane
  }

  /**
    * 0 - left, 1 - right, 2 - top, 3 - bottom
    *
    * @return normals face into the frustum, list of planes, point and normal for each
    */
  def genExtraPlanes(): vector[Plane] =
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

    val normalLeft = lb.crossProduct(nl).normalize()

    val nrb = nvs(1)
    val nrt = nvs(2)
    val frb = fvs(1)

    val rb = nrb - frb
    val nr = nrt - nrb

    val normalRight = rb.crossProduct(nr).normalize()


    val frt = fvs(2)

    val nt = nlt - nrt
    val tr = frt - nrt

    val normalTop = nt.crossProduct(tr).normalize()


    val nb = nrb - nlb

    val normalBottom = nb.crossProduct(-rb).normalize()


    vector[Plane](new Plane(nlb, normalLeft), new Plane(nrb, normalRight), new Plane(frt, normalTop), new Plane(flb, normalBottom))
  }
}


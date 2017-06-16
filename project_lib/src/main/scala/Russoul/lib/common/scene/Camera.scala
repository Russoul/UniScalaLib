package Russoul.lib.common.scene

import Russoul.lib.common.TypeClasses.{CrossProduct, Gram}
import Russoul.lib.common.{Real, Real3, TypeClasses, V4}
import Russoul.lib.common.math.geometry.complex.Frustum
import Russoul.lib.common.math.geometry.simple.{RayOver, RectangleOver}
import Russoul.lib.common.math.algebra.{Mat, Mat4, Vec3, Vec4}
import Russoul.lib.common.Implicits._


class Camera private
{
  var angleOfView = 45D
  var zNear = 0.1D
  var zFar = 300D
  var aspect = 800D/600D



  var pos = Vec3(0D,0D,1.5D)

  //normalized
  var up = Vec3(0D, 1D, 0D)
  //normalized
  var look = Vec3(0D, 0D, -1D)

  var frustum: Frustum = null

  var width = 800D;var height = 600D;var screenX = 0D;var screenY = 0D


  var mat_perspective = Mat4.matrixIdentity()
  var mat_orthographic = Mat4.matrixIdentity()
  var mat_view = Mat4.matrixIdentity()



  def this(width:Real, height:Real, x:Real = 0, y:Real = 0)
  {
    this()

    this.width = width
    this.height = height
    this.aspect = width / height
    this.screenX = x
    this.screenY = y
  }

  def genLookingRay(): RayOver[Real3, Real] =
  {
    new RayOver(pos, look)
  }

  def genRight() = {

    look ⨯ up
  }

  def genNearPlaneRectangle():RectangleOver[Real3, Real] =
  {

    val center = pos + look * zNear

    val t:Real = scala.math.tan(angleOfView/2 * Math.PI / 180)
    val ext1 = t * zNear//width/2
    val v1 = up.⨯(up)
    val ext1v = v1 * (ext1*aspect)
    val ext2v = up * ext1

    new RectangleOver(center, ext1v, ext2v)
  }

  def updateDimensionsAndAspect(w:Real, h:Real) =
  {
    aspect = w/h
    this.width = w
    this.height = h
  }

  def updateScreenPosition(px:Real, py:Real)=
  {
    screenX = px
    screenY = py


  }

  def updateFrustum(): Unit =
  {
    this.frustum = new Frustum(pos, angleOfView, aspect, look, zNear, zFar, up)
  }

  def updateMatrices(): Unit =
  {
    mat_perspective = Mat4.matrixPERSPECTIVE(angleOfView, aspect, zNear, zFar)
    mat_orthographic = Mat4.matrixORTHOGRAPHIC(0, width, 0, height, -1, 1)
    mat_view = Mat4.matrixVIEWDir(pos, look, up)
  }

  def updateTransformation(droll:Real, dyaw:Real, dpitch:Real, dforward:Real, dright:Real, dup:Real): Unit =
  {
    val yaw = dyaw/180*Math.PI
    val pitch = dpitch/180*Math.PI


    val upTransform = Mat4.matrixROTATION(look, -droll)
    var newUp = (up ⨯ upTransform).normalize() //new base : look, newUp, newRight
    var newRight = (look ⨯ newUp).normalize()

    val newLook = (newRight * math.sin(yaw) + look * math.cos(yaw) + newUp * math.sin(pitch)).normalize(); //new look based on dpitch and dyaw in new base
    newRight = (newLook ⨯ newUp).normalize()
    newUp = (newLook ⨯ (-newRight)).normalize()

    look = newLook
    up = newUp

    pos += look * dforward
    pos += newRight * dright
    pos += up * dup

  }

}

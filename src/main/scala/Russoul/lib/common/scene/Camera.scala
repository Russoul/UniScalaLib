package Russoul.lib.common.scene

import Russoul.lib.common.{Mat4F, Real, Real3, Real3F, TypeClasses, V4, Vec3}
import Russoul.lib.common.math.geometry.complex.Frustum
import Russoul.lib.common.math.geometry.simple.{RayOver, RectangleOver}
import Russoul.lib.common.Implicits._


class Camera private
{
  var angleOfView = 45F
  var zNear = 0.1F
  var zFar = 300F
  var aspect = 800F/600F



  var pos = Real3F(0F,0F,1.5F)

  //normalized
  var up = Real3F(0F, 1F, 0F)
  //normalized
  var look = Real3F(0F, 0F, -1F)

  var frustum: Frustum = null

  var width = 800D;var height = 600D;var screenX = 0D;var screenY = 0D


  var mat_perspective = Mat4F.matrixIdentity()
  var mat_orthographic = Mat4F.matrixIdentity()
  var mat_view = Mat4F.matrixIdentity()



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
    mat_perspective = Mat4F.matrixPERSPECTIVE(angleOfView, aspect, zNear, zFar)
    mat_orthographic = Mat4F.matrixORTHOGRAPHIC(0, width, 0, height, -1, 1)
    mat_view = Mat4F.matrixVIEWDir(pos, look, up)
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

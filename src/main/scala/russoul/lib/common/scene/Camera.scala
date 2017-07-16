package russoul.lib.common.scene

import russoul.lib.common.{Mat4F, Real, Real3, Real3F, RealF, TypeClasses, V4, Vec3}
import russoul.lib.common.math.geometry.complex.Frustum
import russoul.lib.common.math.geometry.simple.{RayOver, RectangleOver}
import russoul.lib.common.Implicits._
import russoul.lib.common.math.algebra.Vec
import russoul.lib.common._

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

  var width = 800F;var height = 600F;var screenX = 0F;var screenY = 0F


  var mat_perspective = Mat4F.identity()
  var mat_orthographic = Mat4F.identity()
  var mat_view = Mat4F.identity()



  def this(width:RealF, height:RealF, x:RealF = 0F, y:RealF = 0F)
  {
    this()

    this.width = width
    this.height = height
    this.aspect = width / height
    this.screenX = x
    this.screenY = y
  }

  def genLookingRay(): RayOver[Vec, RealF] =
  {
    RayOver(pos, look)
  }

  def genRight() = {

    look ⨯ up
  }

  def genNearPlaneRectangle():RectangleOver[Vec, RealF] =
  {

    val center = pos + look * zNear

    val t = scala.math.tan(angleOfView/2 * Math.PI / 180).toFloat
    val ext1 = t * zNear//width/2
    val v1 = up.⨯(up)
    val ext1v = v1 * (ext1*aspect)
    val ext2v = up * ext1

    RectangleOver(center, ext1v, ext2v)
  }

  def updateDimensionsAndAspect(w:RealF, h:RealF) =
  {
    aspect = w/h
    this.width = w
    this.height = h
  }

  def updateScreenPosition(px:RealF, py:RealF)=
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
    mat_perspective = Mat4F.perspective(angleOfView, aspect, zNear, zFar)
    mat_orthographic = Mat4F.ortho(0, width, 0, height, -1, 1)
    mat_view = Mat4F.viewDir(pos, look, up)
  }

  def updateTransformation(droll:RealF, dyaw:RealF, dpitch:RealF, dforward:RealF, dright:RealF, dup:RealF): Unit =
  {
    val yaw = dyaw/180*Math.PI.toFloat
    val pitch = dpitch/180*Math.PI.toFloat


    val upTransform = Mat4F.rotationDeg(look, -droll)
    var newUp = transformf(up , upTransform).normalize() //new base : look, newUp, newRight
    var newRight = (look ⨯ newUp).normalize()

    val newLook = (newRight * scala.math.sin(yaw).toFloat + look * scala.math.cos(yaw).toFloat + newUp * scala.math.sin(pitch).toFloat).normalize(); //new look based on dpitch and dyaw in new base
    newRight = (newLook ⨯ newUp).normalize()
    newUp = (newLook ⨯ (-newRight)).normalize()

    look = newLook
    up = newUp

    pos += look * dforward
    pos += newRight * dright
    pos += up * dup

  }

}

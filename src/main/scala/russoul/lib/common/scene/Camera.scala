package russoul.lib.common.scene

import russoul.lib.common._
import russoul.lib.common.math.geometry.complex.Frustum
import russoul.lib.common.math.geometry.simple.{RayOver, RectangleOver}
import russoul.lib.common.Implicits._
import russoul.lib.common.math.algebra.Row
import russoul.lib.common._

import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

class Camera private
{
  var angleOfView = 45F
  var zNear = 0.1F
  var zFar = 300F
  var aspect = 800F/600F



  var pos = Float3(0F,0F,1.5F)

  //normalized
  var up = Float3(0F, 1F, 0F)
  //normalized
  var look = Float3(0F, 0F, -1F)

  var frustum: Frustum = null

  var width = 800F;var height = 600F;var screenX = 0F;var screenY = 0F


  var mat_perspective = Mat4F.identity()
  var mat_orthographic = Mat4F.identity()
  var mat_view = Mat4F.identity()



  def this(width:Float, height:Float, x:Float = 0F, y:Float = 0F)
  {
    this()

    this.width = width
    this.height = height
    this.aspect = width / height
    this.screenX = x
    this.screenY = y
  }

  def genLookingRay(): RayOver[Float] =
  {
    RayOver(pos, look)
  }

  def genRight() = {

    look ⨯ up
  }

  def genNearPlaneRectangle():RectangleOver[Float] =
  {

    val center = pos + look * zNear

    val t = scala.math.tan(angleOfView/2 * Math.PI / 180).toFloat
    val ext1 = t * zNear//width/2
    val v1 = up.⨯(up)
    val ext1v = v1 * (ext1*aspect)
    val ext2v = up * ext1

    RectangleOver(center, ext1v, ext2v)
  }

  def updateDimensionsAndAspect(w:Float, h:Float) =
  {
    aspect = w/h
    this.width = w
    this.height = h
  }

  def updateScreenPosition(px:Float, py:Float)=
  {
    screenX = px
    screenY = py


  }

  def updateFrustum(): Unit =
  {
    this.frustum = Frustum(pos, angleOfView, aspect, look, zNear, zFar, up)
  }

  def updateMatrices(): Unit =
  {
    mat_perspective = Mat4F.perspective(angleOfView, aspect, zNear, zFar)
    mat_orthographic = Mat4F.ortho(0, width, 0, height, -1, 1)
    mat_view = Mat4F.viewDir(pos, look, up)
  }

  def updateTransformation(droll:Float, dyaw:Float, dpitch:Float, dforward:Float, dright:Float, dup:Float): Unit =
  {
    val yaw = dyaw/180*Math.PI.toFloat
    val pitch = dpitch/180*Math.PI.toFloat


    val upTransform = Mat4F.rotationDeg(look, -droll)
    var newUp = transformf(up , upTransform).normalize //new base : look, newUp, newRight
    var newRight = (look ⨯ newUp).normalize

    val newLook = (newRight * scala.math.sin(yaw).toFloat + look * scala.math.cos(yaw).toFloat + newUp * scala.math.sin(pitch).toFloat).normalize; //new look based on dpitch and dyaw in new base
    newRight = (newLook ⨯ newUp).normalize
    newUp = (newLook ⨯ (-newRight)).normalize

    look = newLook
    up = newUp

    pos += look * dforward
    pos += newRight * dright
    pos += up * dup

  }

}

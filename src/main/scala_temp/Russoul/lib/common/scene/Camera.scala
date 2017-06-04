package Russoul.lib.common.scene

import Russoul.lib.common.math.geometry.complex.Frustum
import Russoul.lib.common.math.geometry.simple.{Ray, Rectangle}
import Russoul.lib.common.math.linear.{mat4, vec3, vec4}


class Camera private
{
  var angleOfView = 45
  var zNear = 0.1F
  var zFar = 300F
  var aspect = 800F/600F



  var pos = vec3(0,0,1.5F)

  //normalized
  var up = vec3(0, 1, 0)
  //normalized
  var look = vec3(0, 0, -1)

  var frustum: Frustum = null

  var width = 800F;var height = 600F;var screenX = 0F;var screenY = 0F


  var mat_perspective = mat4.matrixIdentity()
  var mat_orthographic = mat4.matrixIdentity()
  var mat_view = mat4.matrixIdentity()



  def this(width:Float, height:Float, x:Float = 0, y:Float = 0)
  {
    this()

    this.width = width
    this.height = height
    this.aspect = width / height
    this.screenX = x
    this.screenY = y
  }

  def genLookingRay(): Ray =
  {
    new Ray(pos, look)
  }

  def genRight() = look^up

  def genNearPlaneRectangle():Rectangle =
  {
    val center = pos + look * zNear

    val t:Float = scala.math.tan(angleOfView/2 * Math.PI / 180).toFloat
    val ext1 = t * zNear//width/2
    val v1 = up.crossProduct(up)
    val ext1v = v1 * (ext1*aspect)
    val ext2v = up * ext1

    new Rectangle(center, ext1v, ext2v)
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
    this.frustum = new Frustum(pos, angleOfView, aspect, look, zNear, zFar, up)
  }

  def updateMatrices(): Unit =
  {
    mat_perspective = mat4.matrixPERSPECTIVE(angleOfView, aspect, zNear, zFar)
    mat_orthographic = mat4.matrixORTHOGRAPHIC(0, width, 0, height, -1, 1)
    mat_view = mat4.matrixVIEWDir(pos, look, up)
  }

  def updateTransformation(droll:Float, dyaw:Float, dpitch:Float, dforward:Float, dright:Float, dup:Float): Unit =
  {
    val yaw = dyaw/180*Math.PI
    val pitch = dpitch/180*Math.PI

    val upTransform = mat4.matrixROTATION(look, -droll)
    var newUp = (vec4(up, 1) * upTransform).normalize().xyz //new base : look, newUp, newRight
    var newRight = (look ^ newUp).normalize()

    val newLook = (newRight * math.sin(yaw).toFloat + look * math.cos(yaw).toFloat + newUp * math.sin(pitch).toFloat).normalize(); //new look based on dpitch and dyaw in new base
    newRight = (newLook ^ newUp).normalize()
    newUp = (newLook ^ (-newRight)).normalize()

    look = newLook
    up = newUp

    pos += look * dforward
    pos += newRight * dright
    pos += up * dup

  }

}

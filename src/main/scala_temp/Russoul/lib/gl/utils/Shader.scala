package Russoul.lib.gl.utils

import java.util
import java.util.HashMap

import Russoul.lib.common.math.linear.{mat4, vec3, vec4}
import org.lwjgl.opengl.GL20._

/**
  * Created by russoul on 28.01.17.
  */
class Shader(vertex: String, fragment: String) {
  final var ID: Int = ShaderUtils.load(vertex, fragment)
  protected var locationCache: util.HashMap[String, Integer] = new HashMap[String, Integer]
  protected var enabled: Boolean = false


  def getUniform(name: String): Int =
  {
    if (locationCache.containsKey(name)) {
      return locationCache.get(name)
    }
    val result: Int = glGetUniformLocation(ID, name)
    if (result == -1) System.err.println("Could not find uniform variable'" + name + "'!")
    else {
      locationCache.put(name, result)

    }
    result

  }

  def setBool(name: String, value: Boolean): Unit =
  {
    if (!enabled) enable()
    glUniform1i(getUniform(name), if (value) 1 else 0)
  }

  def setInt(name: String, value: Int)
  {
    if (!enabled) enable
    glUniform1i(getUniform(name), value)
  }

  def setFloat(name: String, value: Float)
  {
    if (!enabled) enable
    glUniform1f(getUniform(name), value)
  }

  def setVec2(name: String, x: Float, y: Float)
  {
    if (!enabled) enable
    glUniform2f(getUniform(name), x, y)
  }

  def setVec3(name: String, vector: vec3)
  {
    if (!enabled) enable
    glUniform3f(getUniform(name), vector.x, vector.y, vector.z)
  }

  def setVec4(name: String, vector: vec4)
  {
    if (!enabled) enable
    glUniform4f(getUniform(name), vector.x, vector.y, vector.z, vector.w)
  }

  def setMat4(name: String, matrix: mat4)
  {
    if (!enabled) enable
    glUniformMatrix4fv(getUniform(name), true, matrix.genArray()) //in GLSL matrix-vector multiplication is column vector based !!!, transpose is needed !
  }

  def enable()
  {
    glUseProgram(ID)
    enabled = true
  }

  def disable()
  {
    glUseProgram(0)
    enabled = false
  }
}

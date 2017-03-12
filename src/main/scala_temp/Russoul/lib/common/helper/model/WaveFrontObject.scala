package Russoul.lib.common.helper.model

import Russoul.lib.common.math.immutable.linear.{mat4, vec2, vec3}
import Russoul.lib.common.utils.vector


class WaveFrontObject(name: String)
{
  private val vertexList = new vector[vec3]()
  private val normalList = new vector[vec3]()
  private val textureList = new vector[vec2]()

  private val vertexIndicesTriangleList = new vector[Int]()
  private val texCoordsIndicesTriangleList = new vector[Int]()
  private val normalIndicesTriangleList = new vector[Int]()

  def addVertexNormalIndex(vertexIndex: Int, normalIndex: Int) =
  {
    vertexIndicesTriangleList += vertexIndex
    normalIndicesTriangleList += normalIndex
  }

  def addVertexTextureCoordNormalIndex(pos: Int, tex: Int, n: Int) =
  {
    vertexIndicesTriangleList += pos
    texCoordsIndicesTriangleList += tex
    normalIndicesTriangleList += n
  }

  def addVertex(vertex: vec3) =
  {
    vertexList += vertex
  }

  def addNormal(normal: vec3) =
  {
    normalList += normal
  }

  def addTextureCoord(tex: vec2): Unit =
  {
    textureList += tex
  }

  def getName() = name

  def scale(v3: vec3): WaveFrontObject =
  {
    val mat = mat4.matrixSCALE(v3)
    for (i <- vertexList.indices) {
      val v = vertexList(i)
      val v1 = v.wZero()
      val v2 = v1 * mat
      val v3 = v2.wOff()
      vertexList(i) = v3
    }
    this
  }


}

package Russoul.lib.common.helper.model

import Russoul.lib.common.math.immutable.linear.{mat4, Vec2, Vec3}
import Russoul.lib.common.utils.Arr


class WaveFrontObject(name: String)
{
  private val vertexList = new Arr[Vec3[Float]]()
  private val normalList = new Arr[Vec3[Float]]()
  private val textureList = new Arr[Vec2[Float]]()

  private val vertexIndicesTriangleList = new Arr[Int]()
  private val texCoordsIndicesTriangleList = new Arr[Int]()
  private val normalIndicesTriangleList = new Arr[Int]()

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

  def addVertex(vertex: Vec3[Float]) =
  {
    vertexList += vertex
  }

  def addNormal(normal: Vec3[Float]) =
  {
    normalList += normal
  }

  def addTextureCoord(tex: Vec2[Float]): Unit =
  {
    textureList += tex
  }

  def getName() = name

  def scale(v3: Vec3[Float]): WaveFrontObject =
  {
    val mat = mat4.matrixSCALE(v3)
    for (i <- vertexList.indices) {
      val v = vertexList(i)
      val v1 = v.wZero()
      val v2 = v1 * mat
      val v3 = v2.xyz
      vertexList(i) = v3
    }
    this
  }


}

package Russoul.lib.common.helper.model

import Russoul.lib.common._
import Russoul.lib.common.math.algebra.{Mat, Mat4, Vec2, Vec3}
import Russoul.lib.common.utils.Arr



class WaveFrontObject(name: String)
{
  private val vertexList = new Arr[Vec3[Real]]()
  private val normalList = new Arr[Vec3[Real]]()
  private val textureList = new Arr[Vec2[Real]]()

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

  def addVertex(vertex: Vec3[Real]) =
  {
    vertexList += vertex
  }

  def addNormal(normal: Vec3[Real]) =
  {
    normalList += normal
  }

  def addTextureCoord(tex: Vec2[Real]): Unit =
  {
    textureList += tex
  }

  def getName() = name

  import Implicits._


  def scale(v3: Vec3[Real])(implicit iv3: V3, iv4: V4): WaveFrontObject =
  {
    val mat = Mat4.matrixSCALE(v3)
    vertexList.map(_ ⨯ mat)
    this
  }


}

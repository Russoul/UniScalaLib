package russoul.lib.common.helper.model

import russoul.lib.common._
import russoul.lib.common.utils.Arr


import russoul.lib.common.Implicits._
import spire.algebra._
import spire.math._
import spire.implicits._

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
    val mat = Mat4F.scale(v3)
    vertexList.map(x => {val temp = Float4(x, 0) * mat; Float3(temp(0), temp(1), temp(2))})
    this
  }


}

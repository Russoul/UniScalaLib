package russoul.lib.common.helper.model

import russoul.lib.common._
import russoul.lib.common.utils.Arr
import russoul.lib.common.Implicits._


class WaveFrontObject(name: String)
{
  private val vertexList = new Arr[Vec3[RealF]]()
  private val normalList = new Arr[Vec3[RealF]]()
  private val textureList = new Arr[Vec2[RealF]]()

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

  def addVertex(vertex: Vec3[RealF]) =
  {
    vertexList += vertex
  }

  def addNormal(normal: Vec3[RealF]) =
  {
    normalList += normal
  }

  def addTextureCoord(tex: Vec2[RealF]): Unit =
  {
    textureList += tex
  }

  def getName() = name



  def scale(v3: Vec3[RealF])(implicit iv3: V3, iv4: V4): WaveFrontObject =
  {
    val mat = Mat4F.matrixSCALE(v3)
    vertexList.map(x => {val temp = Float4(x, 0) * mat; Float3(temp.x, temp.y, temp.z)})
    this
  }


}

package Russoul.lib.context.render.gl.shape

import Russoul.lib.common.math.geometry.simple.{AABB, OBB}
import Russoul.lib.common.math.linear.vec3
import Russoul.lib.common.utils.vector
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.{GL11, GL20}
import org.lwjgl.opengl.GL30._
import spire.syntax.cfor._

/**
  * Created by Russoul on 07.08.2016.
  */
class RenderBox
{
  private final val VERTEX_SIZE: Int = 6

  val vertexPool = new vector[Float]
  private val indexPool = new vector[Int]
  private var vcount:Int = 0
  private var constructed = false

  var VBO,VAO,EBO:Int = 0

  val indices = Array[Int](7,3,0, 0,4,7, 4,0,1, 1,5,4, 6,2,3, 3,7,6, 6,7,4, 4,5,6, 3,2,1, 1,0,3, 5,1,2, 2,6,5)

  private def add(v:vector[vec3], color:vec3):Unit =
  {

    cfor(0)(_ < 8, _ + 1){ i =>
      vertexPool += v(i).x
      vertexPool += v(i).y
      vertexPool += v(i).z
      vertexPool += color.x
      vertexPool += color.y
      vertexPool += color.z
    }

    indexPool ++= indices.map(_ + vcount)

    vcount += 8
  }


  def add(obb:OBB, color:vec3): Unit =
  {
    val v = obb.genVertices()

    add(v,color)
  }

  def add(aabb:AABB, color:vec3): Unit =
  {
    val v = aabb.genVertices()

    add(v,color)
  }

  def clearPools() =
  {
    vertexPool.clear()
    indexPool.clear()
    vcount = 0
  }

  def getVertices() = vertexPool

  def getIndices() = indexPool

  def construct(debug:Boolean = false):Boolean =
  {
    if(constructed) return false

    VAO = glGenVertexArrays()
    VBO = glGenBuffers()
    EBO = glGenBuffers()

    glBindVertexArray(VAO)


    glBindBuffer(GL_ARRAY_BUFFER, VBO)
    glBufferData(GL_ARRAY_BUFFER, vertexPool.array, GL_STATIC_DRAW)


    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO)
    glBufferData(GL_ELEMENT_ARRAY_BUFFER,indexPool.array, GL_STATIC_DRAW)


    GL20.glVertexAttribPointer(0, 3, GL11.GL_FLOAT, false, VERTEX_SIZE * 4, 0)
    GL20.glEnableVertexAttribArray(0)

    GL20.glVertexAttribPointer(1, 3, GL11.GL_FLOAT, false, VERTEX_SIZE * 4, 3 * 4)
    GL20.glEnableVertexAttribArray(1)



    glBindBuffer(GL_ARRAY_BUFFER, 0); // Note that this is allowed, the call to glVertexAttribPointer registered VBO as the currently bound vertex buffer object so afterwards we can safely unbind

    glBindVertexArray(0); // Unbind VAO (it's always a good thing to unbind any buffer/array to prevent strange bugs), remember: do NOT unbind the EBO, keep it bound to this VAO

    constructed = true
    true
  }

  def draw(): Unit ={
    if(constructed){
      glBindVertexArray(VAO)
      GL11.glDrawElements(GL11.GL_TRIANGLES, indexPool.size, GL11.GL_UNSIGNED_INT, 0)
      glBindVertexArray(0)
    }
  }

  def deconstruct(): Boolean ={
    if(!constructed) return false

    glDeleteVertexArrays(VAO)
    glDeleteBuffers(VBO)
    glDeleteBuffers(EBO)

    constructed = false
    true
  }
}

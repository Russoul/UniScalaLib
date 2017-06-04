package Russoul.lib.context.render.gl.bounds

import Russoul.lib.common.math.geometry.simple.Line
import Russoul.lib.common.math.linear.vec3
import Russoul.lib.common.utils.vector
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.{GL11, GL20}

/**
  * Created by Russoul on 19.07.2016.
  */
class RenderLines
{
  private final val VERTEX_SIZE: Int = 6

  private val vertexPool = vector[Float]()
  private val indexPool = vector[Int]()
  private var vcount:Int = 0
  private var constructed = false

  var VBO,VAO,EBO:Int = 0

  def addLine(line:Line, color:vec3): Unit =
  {
    vertexPool += line.start.x
    vertexPool += line.start.y
    vertexPool += line.start.z
    vertexPool += color.x
    vertexPool += color.y
    vertexPool += color.z

    vertexPool += line.end.x
    vertexPool += line.end.y
    vertexPool += line.end.z
    vertexPool += color.x
    vertexPool += color.y
    vertexPool += color.z

    indexPool += vcount + 0
    indexPool += vcount + 1
    vcount += 2
  }

  def add(vertexPositions:vector[vec3], color:vec3, indices:vector[Int]): Unit ={
    for(v <- vertexPositions){
      vertexPool ++= v.toArray3f()
      vertexPool ++= color.toArray3f()
    }
    for(i <- indices){
      indexPool += vcount + i
    }

    vcount += vertexPositions.size
  }

  def clearPools() =
  {
    vertexPool.clear()
    indexPool.clear()
    vcount = 0
  }

  def getVertices() = vertexPool

  def getIndices() = indexPool

  def construct():Boolean =
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
      GL11.glDrawElements(GL11.GL_LINES, indexPool.size, GL11.GL_UNSIGNED_INT, 0)
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

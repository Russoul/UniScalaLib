package Russoul.lib.context.scene

import java.nio.ByteBuffer

import org.lwjgl.opengl.{GL30, GL14, GL11}
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryUtil
import spire.math.UByte

/**
  * Created by Russoul on 09.08.2016.
  */
class Texture(val buffer:ByteBuffer, val width:Int, val height:Int)
{
  private val tex:Int = GL11.glGenTextures()

  bind()

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT )
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT )

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)

  unbind()

  def this(width:Int, height:Int)
  {
    this(ByteBuffer.allocateDirect(4*width*height), width, height)
  }

  def getBuffer() = buffer

  def bind() = glBindTexture(GL11.GL_TEXTURE_2D, tex)
  def unbind() = glBindTexture(GL_TEXTURE_2D, 0)

  def loadBinded(): Unit =
  {
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer)
    //GL30.glGenerateMipmap(GL_TEXTURE_2D)
  }

  def loadUnbinded(): Unit =
  {
    bind()
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer)
    //GL30.glGenerateMipmap(GL_TEXTURE_2D)
    unbind()
  }

  def freeBuffer() = MemoryUtil.memFree(buffer)
}

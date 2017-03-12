package Russoul.lib.common.utils

import java.io.{BufferedReader, FileReader, IOException}
import java.nio.{ByteBuffer, ByteOrder, FloatBuffer, IntBuffer}


object Utilities
{

  // This function essentially takes in the location of a shader
  // and then returns it as a string. We need to do this because
  // of the certain way that openGL looks at shader files.
  def loadAsString(location: String): String =
  {
    val result = new StringBuilder()
    try {
      val reader = new BufferedReader(new FileReader(location))
      var buffer = ""
      var ok = true
      while (ok) {
        buffer = reader.readLine()
        if (buffer != null) {
          result.append(buffer)
          result.append("\n")
        }
        else {
          ok = false
        }
      }
      reader.close();

    } catch {
      case e: IOException => System.err.println(e)
    }
    result.toString()
  }

  def createByteBuffer(array: Array[Byte]): ByteBuffer =
  {
    val result = ByteBuffer.allocateDirect(array.length).order(ByteOrder.nativeOrder())
    result.put(array).flip()
    result
  }

  def createFloatBuffer(array: Array[Float]): FloatBuffer =
  {
    val result = ByteBuffer.allocateDirect(array.length << 2).order(ByteOrder.nativeOrder()).asFloatBuffer()
    result.put(array).flip()
    result
  }



  def createIntBuffer(array: Array[Int]): IntBuffer =
  {
    val result = ByteBuffer.allocateDirect(array.length << 2).order(ByteOrder.nativeOrder()).asIntBuffer()
    result.put(array).flip()
    result
  }

}

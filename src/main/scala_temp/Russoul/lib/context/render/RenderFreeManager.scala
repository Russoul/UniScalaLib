package Russoul.lib.context.render

import java.nio.{FloatBuffer, IntBuffer}

import Russoul.lib.common.utils.vector

/**
  * Created by wzlom on 20.08.2016.
  */

trait IRenderer
{
  def free()
}



private[render] object RenderFreeManager
{
  implicit class FBufferMethods(b:FloatBuffer)
  {
    def +=(f:Float) = b.put(f)
  }

  implicit class IBufferMethods(b:IntBuffer)
  {
    def +=(i:Int) = b.put(i)
  }


  private val renderers:vector[IRenderer] = new vector[IRenderer]()
}

package Russoul.lib.gl.utils

import Russoul.lib.common.utils.map

/**
  * Created by russoul on 28.01.17.
  */
object ShaderReg
{
  private val shaders = map.empty[String, Shader]
  private var current:String = _

  def register(vert:String, frag:String, name:String): Unit =
  {
    shaders(name) = new Shader(vert, frag)
  }

  def get(name:String): Shader =
  {
    shaders(name)
  }

  def setCurrent(name:String): Unit =
  {
    current = name
  }

  def getCurrent() = get(current)
}

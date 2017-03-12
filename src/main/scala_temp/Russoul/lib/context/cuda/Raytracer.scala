package Russoul.lib.context.cuda

import Russoul.lib.context.scene.Texture
import org.lwjgl.system.MemoryUtil

/**
  * Created by Russoul on 11.08.2016.
  */
class Raytracer(dirOfPtxFilesToLink:String, kernelName:String, enableExceptions:Boolean, width:Int, height:Int)
{
  CudaLib.enableExceptions(enableExceptions)
  val cuda = new CudaProcess(dirOfPtxFilesToLink)
  var func = cuda.getFunction(kernelName)

  var size = 4*width*height

  var texture:Texture = new Texture(MemoryUtil.memAlloc(4*width*height), width, height)


  def free(): Unit =
  {
    texture.freeBuffer()
  }

}


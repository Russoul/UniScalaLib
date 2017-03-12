package Russoul.lib.context.utils.native


import scala.concurrent.util.Unsafe.{instance => native}
import scala.reflect.ClassTag

/**
  * Created by Russoul on 02.08.2016.
  */
trait NativeAllocator[T]
{
  var cachedSize:Option[(Long,Long)] = None
  def gecSize() =
  {
    if(cachedSize.isEmpty) cachedSize = Option(NativeManager.sizeOf(classOf[ClassTag[T]]))

    cachedSize
  }

  def alloc(): Long ={
    native.allocateMemory(gecSize().get._1)
  }

  def dealloc(ptr:Long) =
  {
    native.freeMemory(ptr)
  }
}

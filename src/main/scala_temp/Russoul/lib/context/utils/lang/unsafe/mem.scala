package Russoul.lib.context.utils.lang.unsafe

import java.nio.{Buffer, FloatBuffer, ByteBuffer}

import Russoul.lib.common.math.immutable.linear.{vec4, vec2, vec3}
import jcuda.{NativePointerObject, Pointer}
import jcuda.driver.CUdeviceptr

import scala.concurrent.util.Unsafe
import scala.reflect.ClassTag

/**
  * Created by Russoul on 09.08.2016.
  */



trait FloatBytesAccess
{
  protected[unsafe] val array:Array[Float]
}

object mem
{

  //Initialising reflection
  private final val c = classOf[Pointer]
  private final val f = c.getDeclaredField("pointers")
  f.setAccessible(true)
  //........................


  @throws(classOf[Exception])
  def addressOf(o: AnyRef): Long =
  {
    val unsafe = Unsafe.instance

    val array: Array[AnyRef] = Array[AnyRef](o)
    val baseOffset: Long = unsafe.arrayBaseOffset(classOf[Array[AnyRef]])
    val addressSize: Int = unsafe.addressSize
    var objectAddress: Long = 0L
    addressSize match {
      case 4 =>
        objectAddress = unsafe.getInt(array, baseOffset)
      case 8 =>
        objectAddress = unsafe.getLong(array, baseOffset)
      case _ =>
        throw new Error("unsupported address size: " + addressSize)
    }
    objectAddress
  }

  @throws(classOf[Exception])
  def getAddress[T](obj:T, field:String)(implicit t : ClassTag[T]): Long =
  {
    val f = t.runtimeClass.getDeclaredField(field)
    val offsetField = Unsafe.instance.objectFieldOffset(f)
    val ad = addressOf(obj.asInstanceOf[AnyRef])
    ad + offsetField
  }


  /**
    * reflection, slow
    *
    * @param ptrs
    * @return
    */
  def &(ptrs:NativePointerObject*)=
  {
    val ar:Array[NativePointerObject] = ptrs.toArray
    val p = new Pointer()
    f.set(p, ar)
    p
  }

  def &(ptr:CUdeviceptr) = Pointer.to(ptr)
  def &(ptr:Array[Float]) = Pointer.to(ptr)
  def &(ptr:Array[Int]) = Pointer.to(ptr)
  def &(ptr:Array[Byte]) = Pointer.to(ptr)
  def &(ptr:Array[Long]) = Pointer.to(ptr)
  def &[T <: Buffer](buf: T) = Pointer.to(buf)


  def &(floatBytesAccess: FloatBytesAccess) = Pointer.to(floatBytesAccess.array)

  def ~(floatBytesAccess: FloatBytesAccess):Array[Float] = floatBytesAccess.array
}


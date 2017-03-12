package Russoul.lib.context.utils.native

import java.lang.reflect.Modifier

import scala.concurrent.util.Unsafe

/**
  * Created by Russoul on 02.08.2016.
  */
object NativeManager
{

  /**
    *
    * @param clazzIn
    * @tparam T
    * @return 1) size without class's meta information 2) size of meta information
    */
  def sizeOf[T <: AnyRef](clazzIn: Class[T]): (Long, Long) =
  {
    var maximumOffset: Long = 0
    var minimumOffset: Long = 999
    var first = true
    var clazz: Class[_ >: T] = clazzIn

    while (first || clazz.getSuperclass != null) {
      if (first) {
        first = false
      } else {
        clazz = clazz.getSuperclass
      }

      for(f <- clazz.getDeclaredFields) {
        if(!Modifier.isStatic(f.getModifiers)) {
          val off = Unsafe.instance.objectFieldOffset(f)
          maximumOffset = Math.max(maximumOffset, off)
          minimumOffset = Math.min(minimumOffset, off)
        }
      }


    }

    (maximumOffset + 8 -minimumOffset, minimumOffset)
  }

  /*def place[T <: AnyRef](o:T, address:Long) = {
    val metaSize = sizeOf(classOf[T])._1

    var first = true
    var clazz: Class[_ >: T] = classOf[T]

    while (first || clazz.getSuperclass != null) {
      if (first) {
        first = false
      } else {
        clazz = clazz.getSuperclass
      }

      for(f <- clazz.getDeclaredFields) {
        if(!Modifier.isStatic(f.getModifiers)) {
          val off = Unsafe.instance.objectFieldOffset(f)



          f.getType match
          {
            case long if long.isAssignableFrom(classOf[Long])     => Unsafe.instance.putLong(address + off-metaSize, f.get(o).asInstanceOf[Long])
            case int if int.isAssignableFrom(classOf[Int])        => Unsafe.instance.putInt(address + off-metaSize, f.get(o).asInstanceOf[Int])
            case float if float.isAssignableFrom(classOf[Float])  => Unsafe.instance.putFloat(address + off-metaSize, f.get(o).asInstanceOf[Float])
            case _ => throw new UnsupportedOperationException
          }
        }
      }

    }
  }

  def read[T <: AnyRef](address:Long):T = {
    val metaSize = sizeOf(classOf[T])._1
    val obj = Unsafe.instance.allocateInstance(classOf[T])

    var first = true
    var clazz: Class[_ >: T] = classOf[T]

    while (first || clazz.getSuperclass != null) {
      if (first) {
        first = false
      } else {
        clazz = clazz.getSuperclass
      }

      for(f <- clazz.getDeclaredFields) {
        if(!Modifier.isStatic(f.getModifiers)) {
          val off = Unsafe.instance.objectFieldOffset(f)



          f.getType match
          {
            case long if long.isAssignableFrom(classOf[Long])     => Unsafe.instance.putLong(obj, off, Unsafe.instance.getLong(address + off - metaSize))
            case int if int.isAssignableFrom(classOf[Int])        => Unsafe.instance.putInt(obj, off, f.get(o).asInstanceOf[Int])
            case float if float.isAssignableFrom(classOf[Float])  => Unsafe.instance.putFloat(obj, off, f.get(o).asInstanceOf[Float])
            case _ => throw new UnsupportedOperationException
          }
        }
      }

    }
  }*/


}

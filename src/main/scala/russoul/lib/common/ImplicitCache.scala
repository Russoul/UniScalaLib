package russoul.lib.common

import scala.collection.mutable
import scala.reflect.ClassTag

object ImplicitCache {


  private final val cache = new mutable.HashMap[ClassTag[_], AnyRef]()

  @inline def cache[T <: AnyRef](impl : => T)(implicit tag : ClassTag[T]) : T = {
    val found = cache.get(tag)
    found match{
      case Some(x) => x.asInstanceOf[T]
      case None => val newImpl = impl; cache.put(tag, newImpl); newImpl
    }
  }
}

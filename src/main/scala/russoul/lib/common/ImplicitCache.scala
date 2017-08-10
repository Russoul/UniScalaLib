package russoul.lib.common

import scala.collection.mutable
import scala.reflect.runtime.universe._

object ImplicitCache {


  private final val cache = new mutable.HashMap[TypeTag[_], AnyRef]()

  @inline def cache[T <: AnyRef](impl : => T)(implicit tag : TypeTag[T]) : T = {
    println(tag)
    val found = cache.get(tag)
    found match{
      case Some(x) => x.asInstanceOf[T]
      case None => val newImpl = impl; cache.put(tag, newImpl); newImpl
    }
  }
}

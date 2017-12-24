package russoul.lib.common.utils

import russoul.lib.common.{immutable, tbsp}

import scala.reflect.ClassTag

/**
  * Created by russoul on 18.06.2017.
  *
  * immutable Array container
  * cannot be empty
  *
  *
  */

//TODO probably remove it
@immutable class ImArr[@tbsp T : ClassTag] private (private val array: Array[T]) {

  def apply(index:Int): T = array(index)

  override def hashCode(): Int = {
    var hash = 0
    var i = 0
    while(i < array.size){
      hash += array(i).## * 11
      i += 1
    }

    hash
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match{
      case that : ImArr[T] =>
       if(that.array.size == this.array.size){
         var eq = true
         var i = 0
         while(i < array.size && eq){
           if(array(i)  != that.array(i)) eq = false
           i += 1
         }

         eq
       }else
         false
      case _ => false
    }
  }
}
object ImArr{
  def apply[@tbsp T : ClassTag](seq: T*) : ImArr[T] = new ImArr[T](seq.toArray[T])
  def apply[@tbsp T : ClassTag](array: Array[T]) : ImArr[T] = new ImArr[T](array)
}

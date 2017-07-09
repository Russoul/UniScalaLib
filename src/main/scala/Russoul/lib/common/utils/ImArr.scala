package Russoul.lib.common.utils

import Russoul.lib.common.immutable

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
@immutable class ImArr[@specialized T : ClassTag](private val array: Array[T]) {

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
    }
  }
}
object ImArr{
  def apply[@specialized T : ClassTag](seq: T*) : ImArr[T] = new ImArr[T](seq.toArray[T])
}

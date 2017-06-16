package Russoul.lib.common.utils

import scala.reflect.ClassTag


class KeyNotFoundException(k: String) extends Exception("key %s was not found" format k)


/**
  * Created by russoul on 28.01.17.
  *
  * very simple and slow implementation of map, get = O(n)
  */


class Map[@specialized A, @specialized B](var keys:Arr[A], var values:Arr[B])(implicit val cta: ClassTag[A], val ctb: ClassTag[B])
{
  def this()(implicit cta: ClassTag[A], ctb: ClassTag[B]) =
  {
    this(new Arr[A](), new Arr[B]())

  }

  def size = keys.size

  /**
    * Check if two Maps are equal.
    *
    * Equal means the maps have the same types (which is checked
    * using the ClassTag instances) and the same contents.
    *
    * Comparing Maps with any of Scala's collection types will
    * return false.
    */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: Map[_, _] =>
      if (this.size != rhs.size || this.cta != rhs.cta || this.ctb != rhs.ctb) return false
      val m = rhs.asInstanceOf[Map[A, B]]
      if(this.keys.equals(m.keys) && this.values.equals(m.values)) true else false
    case _ =>
      false
  }



  /**
    * Hash the contents of the map to an Int value.
    *
    * By xor'ing all the map's keys and values together, we can be sure
    * that maps with the same contents will have the same hashCode
    * regardless of the order those items appear.
    *
    * This is an O(n) operation.
    */
  override def hashCode: Int = {
    var n: Int = 0xb0bd0bb5
    for(i <- 0 until size){
      n ^= keys(i).## * values(i).##
    }
    n
  }

  /**
    * Return a string representation of the contents of the map.
    *
    * This is an O(n) operation.
    */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("map( ")

    if(size == 0){
      sb.append(" )")
      sb.toString
    }
    else if(size == 1){
      sb.append("(")
      sb.append(keys(0).toString)
      sb.append(" -> ")
      sb.append(values(0).toString)
      sb.append(")")

      sb.append(" )")
      sb.toString
    }else{
      sb.append("(")
      sb.append(keys(0).toString)
      sb.append(" -> ")
      sb.append(values(0).toString)
      sb.append(")")


      for(i <- 1 until size){
        sb.append(" ,")

        sb.append("(")
        sb.append(keys(i).toString)
        sb.append(" -> ")
        sb.append(values(i).toString)
        sb.append(")")

        sb.append(" )")

      }

      sb.toString
    }


  }



  /**
    * Return true if the Map is empty, false otherwise.
    *
    * This is an O(1) operation.
    */
  final def isEmpty: Boolean = size == 0

  /**
    * Return true if the Map is non-empty, false otherwise.
    *
    * This is an O(1) operation.
    */
  final def nonEmpty: Boolean = size > 0

  final def update(key: A, value: B): Unit =
  {
    val index = getIndex(key)

    if(index == -1){
      keys.append(key)
      values.append(value)
    }else{
      values(index) = value
    }
  }


  def apply(key:A):B = get(key).get

  def get(key: A):Option[B] =
  {
    for(i <- 0 until size){
      val k = keys(i)
      if(key == k){
        return Some(values(i))
      }
    }

    None

  }

  def clear(): Unit =
  {
    keys = new Arr[A]()
    values = new Arr[B]()
  }

  def remove(key:A):Boolean  =
  {
    val i = getIndex(key)

    if(i >= 0){
      keys.remove(i)
      values.remove(i)
      true
    }else{
      false
    }

  }

  def getIndex(key: A):Int =
  {
    for(i <- 0 until size){
      val k = keys(i)
      if(key == k){
        return i
      }
    }

    -1

  }

  def getOrElse(key: A)( callback: (Unit => Unit) ):B =
  {
    for(i <- 0 until size){
      val k = keys(i)
      if(key == k){
        return values(i)
      }
    }

    null.asInstanceOf[B]

  }

  final def contains(key: A): Boolean = {
    keys.contains(key)
  }
}

object Map
{
  def empty[@specialized A:ClassTag,@specialized B:ClassTag](): Map[A,B] =
  {
    new Map[A,B]()
  }
}

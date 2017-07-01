package Russoul.lib.common.utils

import Russoul.lib.common.math.Math
import Russoul.lib.common.mutable

import scala.math.Ordering
import scala.reflect.ClassTag

/**
  * Created by russoul on 28.01.17.
  *
  * mutable fast implementation of Vector
  */
@mutable class Arr[@specialized T](var array:Array[T], var size:Int)(implicit val ct: ClassTag[T])
{


  def discard() = size = 0

  /**
    *
    * match size with internal(allocated) size
    */
  def equaliseSizes() = size = array.size

  def apply(index:Int) = array(index)

  def this()(implicit ct: ClassTag[T]) =
  {
    this(new Array[T](Arr.DEFAULT_SIZE), 0)
  }

  def this(args:T*)(implicit ct: ClassTag[T]) = //exact amount
  {
    this(new Array[T](args.size), args.size)

    setSeq(args)
  }

  def length() = size

  private def setSeq(seq:Seq[T]): Unit =
  {
    for(i <- indices()){
      array(i) = seq(i)
    }
  }

  def this(allocThisMuch:Int)(implicit ct: ClassTag[T])=
  {
    this(new Array[T](allocThisMuch), 0)
  }

  /**
    * Check if two Buffers are equal.
    *
    * Equal means the buffers have the same type (which is checked
    * using the ClassTag instances) and the same contents.
    *
    * Comparing Buffers with any of Scala's collection types will
    * return false.
    */
  override def equals(that: Any): Boolean = that match {
    case b: Arr[_] =>
      if (size != b.size || ct != b.ct) return false
      val buf = b.asInstanceOf[Arr[T]]
      for(i <- 0 until size){
        if(buf.array(i) != array(i)) return false
      }
      true
    case _ =>
      false
  }

  /**
    * Hash the contents of the buffer to an Int value.
    */
  override def hashCode(): Int = {
    var code: Int = 0xf457f00d

    for(i <- 0 until size){
      code = (code * 19) + array(i).##
    }

    code
  }


  // Ordering[T] might be slow especially for boxed primitives, so use binary search variant of insertion sort
  // Caller must pass iN >= i0 or math will fail.  Also, i0 >= 0.
  private def insertionSort(a: Array[T], i0: Int, iN: Int, ord: Ordering[T]): Unit = {
    val n = iN - i0
    if (n < 2) return
    if (ord.compare(a(i0), a(i0+1)) > 0) {
      val temp = a(i0)
      a(i0) = a(i0+1)
      a(i0+1) = temp
    }
    var m = 2
    while (m < n) {
      // Speed up already-sorted case by checking last element first
      val next = a(i0 + m)
      if (ord.compare(next, a(i0+m-1)) < 0) {
        var iA = i0
        var iB = i0 + m - 1
        while (iB - iA > 1) {
          val ix = (iA + iB) >>> 1    // Use bit shift to get unsigned div by 2
          if (ord.compare(next, a(ix)) < 0) iB = ix
          else iA = ix
        }
        val ix = iA + (if (ord.compare(next, a(iA)) < 0) 0 else 1)
        var i = i0 + m
        while (i > ix) {
          a(i) = a(i-1)
          i -= 1
        }
        a(ix) = next
      }
      m += 1
    }
  }

  def insertionSort(ord:Ordering[T]): Arr[T] =
  {
    insertionSort(array, 0, size, ord)
    this
  }


  /**
    * Return a string representation of the contents of the buffer.
    */
  override def toString() = {
    if (size == 0) {
      "vector()"
    } else {
      val sb = new StringBuilder()
      sb.append("vector(")
      sb.append(apply(0))
      for (i <- 1 until size) {
        sb.append(", ")
        sb.append(array(i))
      }
      sb.append(")")
      sb.toString
    }
  }

  /**
    * Copy the buffer's contents to a new buffer.
    */
  final def copy: Arr[T] = new Arr(array.clone, size)

  def growIfNecessary(delta: Int): Unit = {
    val goal = size + delta
    val alloc = array.length
    if (alloc >= goal) return

    var x = if (alloc == 0) 8 else Math.nextPowerOfTwo(alloc + 1)
    while (x >= 0 && x < goal) x = Math.nextPowerOfTwo(x + 1)
    if (x < 0) throw new Exception(""+x)
    grow(x)

  }

  def grow(n: Int): Unit = {
    val arr = new Array[T](n)
    System.arraycopy(array, 0, arr, 0, size)
    array = arr //last mem will be GCed automatically

  }

  /**
    * Return true if the Buffer is empty, false otherwise.
    *
    * This is an O(1) operation.
    */
  def isEmpty: Boolean = size == 0

  /**
    * Return true if the Buffer is non-empty, false otherwise.
    *
    * This is an O(1) operation.
    */
  def nonEmpty: Boolean = size > 0

  /**
    * Update the value of element i.
    *
    * This method has similar caveats to apply. If an illegal i value
    * is used, an ArrayIndexOutOfBoundsException may be thrown. If no
    * exception is thrown, the update will have been ignored. Under no
    * circumstances will an invalid index corrupt the buffer.
    *
    * This is an O(1) operation.
    */
  def update(i: Int, a: T): Unit = array(i) = a

  /**
    * This method is a synonym for append.
    */
  def append(a: T): Unit = this += a

  /**
    * Append a new value to the end of the buffer.
    *
    * If there is no space left in the underlying array this method
    * will trigger a grow, increasing the underlying storage
    * capacity.
    *
    * This is an amortized O(1) operation.
    */
  def +=(a: T): Unit = {
    if (size >= array.length) grow(Math.nextPowerOfTwo(size + 1))
    array(size) = a
    size = size + 1
  }

  /**
    * Insert a new value at index i.
    *
    * For i values that are negative, or greater than the length of the
    * buffer, an exception will be thrown. If i == buffer.length, the
    * value will be appended to the end. Otherwise, this method will
    * shift the values at i and beyond forward to make room.
    *
    * This is an O(n) operation, where n is buffer.length.
    */
  def insert(i: Int, a: T): Unit =
    if (i < 0 || i > size) {
      throw new IllegalArgumentException(i.toString)
    } else if (i == size) {
      append(a)
    } else {
      growIfNecessary(1)
      System.arraycopy(array, i, array, i + 1, size - i)
      array(i) = a
      size += 1
    }

  /**
    * Insert a new value at the beginning of the buffer.
    *
    * This method will shift the contents of the buffer forward to make
    * space for the new value.
    *
    * This is an O(n) operation, where n is buffer.length.
    */
  def prepend(a: T): Unit = insert(0, a)

  /**
    * This is a synonym for ++.
    */
  def concat(buf: Arr[T]): Arr[T] = this ++ buf

  /**
    * Concatenate two buffers, returning a new buffer.
    *
    * This method does not modify either input buffer, but allocates
    * and returns a new one.
    *
    * This is an O(n+m) operation, where n and m are the lengths of the
    * input buffers.
    */
  def ++(buf: Arr[T]): Arr[T] = {
    val result = this.copy; result ++= buf; result
  }

  /**
    * This is a synonym for ++=.
    */
  def extend(arr: Array[T]): Unit = this ++= arr

  /**
    * This is a synonym for extend.
    */
  def extend(buf: Arr[T]): Unit = this ++= buf

  /**
    * This is a synonym for extend.
    */
  def extend(items: Iterable[T]): Unit = this ++= items

  /**
    * Append the values in arr to the end of the buffer.
    *
    * This method is an O(m) operation, where m is the length of arr.
    */
  def ++=(arr: Array[T]): Unit = splice(size, arr)

  /**
    * Append the values in buf to the end of the buffer.
    *
    * This method is an O(m) operation, where m is the length of buf.
    */
  def ++=(buf: Arr[T]): Unit = splice(size, buf)

  /**
    * Append the values in elems to the end of the buffer.
    *
    * This method is an O(m) operation, where m is the length of items.
    */
  def ++=(items: Iterable[T]): Unit = items.foreach(append)

  /**
    * Splice the values in arr into the buffer at index i.
    *
    * If i is negative or greater than buffer.length, an exception will
    * be thrown. If i is equal to buffer.length, the buffer will be
    * extended with arr. Otherwise, this method will shift the elements
    * at i and beyond forward to make room for arr's elements. Thus,
    * the size of the buffer will increase by arr.length.
    *
    * This method is an O(m+n) operation, where m is the length of arr,
    * and n is the length of the buffer.
    */
  def splice(i: Int, arr: Array[T]): Unit =
    if (i < 0 || i > size) {
      throw new IllegalArgumentException(i.toString)
    } else {
      val n = arr.length
      growIfNecessary(n)
      if (i < size) System.arraycopy(array, i, array, i + n, size - i)
      System.arraycopy(arr, 0, array, i, n)
      size += n
    }

  /**
    * Splice the values in buf into the buffer at index i.
    *
    * If i is negative or greater than buffer.length, an exception will
    * be thrown. If i is equal to buffer.length, the buffer will be
    * extended with buf. Otherwise, this method will shift the elements
    * at i and beyond forward to make room for buf's elements. Thus,
    * the size of the buffer will increase by buf.length.
    *
    * This method is an O(m+n) operation, where m is the length of buf,
    * and n is the length of the buffer.
    */
  def splice(i: Int, buf: Arr[T]): Unit =
    if (i < 0 || i > size) {
      throw new IllegalArgumentException(i.toString)
    } else {
      val n = buf.size
      growIfNecessary(n)
      if (i < size) System.arraycopy(array, i, array, i + n, size - i)
      System.arraycopy(buf.array, 0, array, i, n)
      size += n
    }


  /**
    * Prepend the values from arr into the beginning of the buffer.
    *
    * Like splice, this method will shift all the buffer's values back
    * to make room.
    *
    * This method is an O(m+n) operation, where m is the length of arr,
    * and n is the lenght of the buffer.
    */
  def prependAll(arr: Array[T]): Unit = splice(0, arr)

  /**
    * Prepend the values from arr into the beginning of the buffer.
    *
    * Like splice, this method will shift all the buffer's values back
    * to make room.
    *
    * This method is an O(m+n) operation, where m is the length of arr,
    * and n is the lenght of the buffer.
    */
  def prependAll(buf: Arr[T]): Unit = splice(0, buf)

  /**
    * Remove the element at i, returning the value removed.
    *
    * This method verifies that the index i is valid; if not, it will
    * throw an exception.
    *
    * This method is an O(n) operation, where n is buffer.length.
    * Removing the last element of the buffer is O(1) operation, and
    * can also be accomplished with pop.
    */
  def remove(i: Int): T = {
    val last = size - 1
    if (i < 0) {
      throw new IndexOutOfBoundsException(i.toString)
    } else if (i < last) {
      System.arraycopy(array, i + 1, array, i, last - i)
      val a = array(last)
      array(last) = null.asInstanceOf[T]
      size = last
      a
    } else if (i == last) {
      pop
    } else {
      throw new IndexOutOfBoundsException(i.toString)
    }
  }

  /**
    * Remove the last element, returning the value returned.
    *
    * If the buffer is empty, this method throws an exception.
    *
    * This method is an O(1) operation.
    */
  def pop: T =
    if (size > 0) {
      val last = size - 1
      val a = array(last)
      size = last
      a
    } else {
      throw new IndexOutOfBoundsException("0")
    }


  /**
    * Remove the elements starting at i to i + num - 1
    *
    * This method verifies that the index i is valid; if not, it will
    * throw an exception.
    *
    * This method is an O(n) operation, where n is buffer.length.
    * Removing the last element of the buffer is O(1) operation, and
    * can also be accomplished with pop.
    */
  def remove(i:Int, num: Int): Unit =
  {
    val last = size - 1
    if (i < 0) {
      throw new IndexOutOfBoundsException(i.toString)
    } else if (i + num - 1 < last) {
      System.arraycopy(array, i + num , array, i, last - i - num + 1)
      for(i <- last - num + 1 to last)
        array(i) = null.asInstanceOf[T]
      size = last - num + 1
    } else {
      throw new IndexOutOfBoundsException( (i+num-1).toString)
    }
  }



  final def contains(obj: T): Boolean =
  {
    for(v <- this){
      if(v == obj) return true
    }

    false
  }

  final def find(obj: T) : Option[Int] = {
    for(i <- indices()){
      if(this(i) == obj) return Some(i)
    }

    None
  }

  def map(f:Function[T,T]): Unit =
  {
    map(indices(), f)
  }

  def map(range:Range, f:Function[T,T]) : Unit =
  {
    for(i <- range){
      map(i, f)
    }
  }

  @inline def map(index:Int, f:Function[T,T]) : Unit =
  {
    array(index) = f(array(index))
  }

  def foreach(f: Function[T, Unit]): Unit =
  {
    for(i <- indices) f(array(i))

  }

  def exists(f:Function[T, Boolean]) : Boolean =
  {
    for(v <- this) if(f(v)) return true

    false
  }

  def indices() = 0 until size

  def absorb(vec:Arr[T]): Unit =
  {
    this.array = vec.array
    this.size = vec.size
  }

  def clear(): Unit =
  {
    absorb(Arr.empty[T]())
  }

  def toImmutable(): ImArr[T] = {
    new ImArr[T](array.clone())
  }
}

object Arr
{
  private final val DEFAULT_SIZE = 8

  /**
    * Allocate an empty Buffer.
    */
  def emptyOptimum[@specialized T: ClassTag](): Arr[T] =
    ofSizeOptimum[T](DEFAULT_SIZE)

  def empty[@specialized T: ClassTag](): Arr[T] =
    ofSize[T](0)

  /**
    * Allocate an empty Buffer, capable of holding n items without
    * resizing itself.
    *
    * This method is useful if you know you'll be adding a large number
    * of elements in advance and you want to save a few resizes.
    */
  def ofSizeOptimum[@specialized T: ClassTag](n: Int): Arr[T] =
    new Arr(new Array[T](Math.nextPowerOfTwo(n)), 0)

  def ofSize[@specialized T: ClassTag](n: Int): Arr[T] =
    new Arr(new Array[T](n), 0)

  def apply[@specialized T: ClassTag](): Arr[T] =
  {
    new Arr[T]()
  }

  def apply[@specialized T: ClassTag](allocSize:Int): Arr[T] =
  {
    new Arr[T](allocSize)
  }

  def apply[@specialized T: ClassTag](args:T*): Arr[T] =
  {
    val re = new Arr[T](args.size)

    for(i <- 0 until args.size){
      re(i) = args(i)
    }

    re.size = args.size

    re
  }

}

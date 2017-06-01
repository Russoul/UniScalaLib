

import scala.annotation.Annotation
import scala.annotation.meta.param
import scala.language.experimental.macros
/**
  * Created by russoul on 01.04.17.
  */

import scala.language.implicitConversions




import scala.math.Ordering
import scala.reflect.ClassTag

/**
  * Created by russoul on 18.05.17.
  */
object TypeClasses {

  type Real = Double

  trait Addable[A] {
    @inline def plus(x: A, y:A): A

    override def toString: String

    class AddableOps(lhs: A)(implicit ev: Addable[A]){
      def +(rhs: A): A = ev.plus(lhs, rhs)
    }

    //implicit def mkAddableOps(lhs: A): AddableOps = new AddableOps(lhs)
  }

  object Addable {
    object Implicits{
      implicit def infixAddableOps[A](x: A)(implicit num: Addable[A]): Addable[A]#AddableOps = new num.AddableOps(x)
    }
  }

  trait CommutativeGroup[A] extends Addable[A] with Ordering[A]{
    @inline def negate(x: A): A
    @inline def zero: A

    @inline def abs(x:A): A = {
      val neg = negate(x)

      if(lt(x,neg)){
        neg
      }else
        x
    }

    @inline def minus(x: A, y: A): A = plus(x, negate(y))

    class CommutativeGroupOps(lhs: A)(implicit ev: CommutativeGroup[A]) extends AddableOps(lhs){
      def -(rhs: A): A = ev.minus(lhs, rhs)
      def unary_-(): A = ev.negate(lhs)

    }

    //implicit def mkCommutativeGroupOps(lhs: A): CommutativeGroupOps = new CommutativeGroupOps(lhs)

  }

  object CommutativeGroup {
    object Implicits{
      implicit def infixCommutativeGroupLikeOps[A](x: A)(implicit num: CommutativeGroup[A]): CommutativeGroup[A]#CommutativeGroupOps = new num.CommutativeGroupOps(x)


    }
  }

  trait Trig[A]{
    @inline def atan2(x:A, y:A) : A
    @inline def toRadians(x:A): A
    @inline def cos(x:A): A
    @inline def sin(x:A): A
    @inline def acos(x:A):A
  }

  trait RealPow[A]
  {
    @inline def sqrt(x:A) : A
    @inline def pow(x: A, y:A): A
  }

  trait ConvertibleFromDouble[A]{
    @inline def fromDouble(x: Double) : A
  }

  trait Field[A] extends CommutativeGroup[A]{
    @inline def times(x: A, y: A): A
    @inline def inv(x: A): A
    @inline def one:A
    @inline def div(x: A, y: A): A = times(x, inv(y))



    class FieldOps(lhs: A)(implicit ev: Field[A]) extends CommutativeGroupOps(lhs){
      def *(rhs: A) : A = ev.times(lhs, rhs)
      def /(rhs: A) : A = ev.div(lhs, rhs)



    }

    //implicit def mkFieldOps(lhs: A): FieldOps = new FieldOps(lhs)
  }

  abstract class VectorSpaceOverField[V,F](implicit ev: Field[F]) {
    @inline def plus(a:V, b:V) : V
    @inline def negate(a:V):V
    @inline def times(a:V, k:F):V
    @inline def zero : V

    @inline def divide(a:V, k:F):V = times(a, ev.inv(k))
    @inline def minus(a:V, b:V):V = plus(a, negate(b))

  }

  abstract class EuclideanSpaceOverField[V,F](implicit ev: Field[F] with Trig[F] with RealPow[F]) extends VectorSpaceOverField[V,F]{
    @inline def dotProduct(a:V, b:V, basis:Arr[V]):F

    @inline def length(a:V, basis:Arr[V]):F = ev.sqrt(dotProduct(a,a, basis))
    @inline def angle(a:V, b:V, basis:Arr[V]):F = ev.acos(ev.div(ev.div(dotProduct(a, b, basis), length(a, basis)), length(b, basis)))

    @inline def canonicalBasis : Arr[V]
  }

  object Field {
    object Implicits{
      implicit def infixFieldLikeOps[A](x: A)(implicit num: Field[A]): Field[A]#FieldOps = new num.FieldOps(x)

      //TODO this results in a cast overhead !!!
      @inline implicit def to[A](x:Double)(con: ConvertibleFromDouble[A]): A = con.fromDouble(x)

      implicit class DoubleToSmth(x:Double){
        @inline def to[A](implicit con: ConvertibleFromDouble[A]): A = con.fromDouble(x)
      }
    }
  }


  trait IntIsCommutativeGroup extends CommutativeGroup[Int]{
    override def negate(x: Int): Int = -x
    override def zero: Int = 0
    override def plus(x: Int, y: Int): Int = x + y

    override def toString: String = {
      "Int"
    }

  }

  trait FloatIsTrig extends Trig[Float]{
    override def atan2(x: Float, y:Float): Float = math.atan2(x,y).toFloat

    override def cos(x: Float): Float = math.cos(x).toFloat
    override def sin(x: Float): Float = math.sin(x).toFloat


    override def acos(x: Float): Float = math.acos(x).toFloat

    override def toRadians(x: Float): Float = {
      math.toRadians(x).toFloat
    }
  }



  trait FloatIsConvertibleFromDouble extends ConvertibleFromDouble[Float]{
    override def fromDouble(x: Double): Float = x.toFloat
  }

  trait DoubleIsConvertibleFromDouble extends ConvertibleFromDouble[Double]{
    override def fromDouble(x: Double): Double = x
  }

  trait FloatIsRealPow extends RealPow[Float]{
    override def sqrt(x: Float): Float = math.sqrt(x).toFloat
    override def pow(x: Float, y:Float): Float = math.pow(x,y).toFloat
  }

  trait FloatIsField extends Field[Float]{
    override def times(x: Float, y: Float): Float = x * y
    override def inv(x: Float): Float = 1/x
    override val one: Float = 1F
    override def negate(x: Float): Float = -x
    override val zero: Float = 0F
    override def plus(x: Float, y: Float): Float = x + y
    override def toString: String = {
      "Float"
    }

  }


  trait DoubleIsTrig extends Trig[Double]{
    override def atan2(x: Double, y:Double): Double = math.atan2(x,y)

    override def cos(x: Double): Double = math.cos(x)
    override def sin(x: Double): Double = math.sin(x)


    override def acos(x: Real): Real = math.acos(x)

    override def toRadians(x: Double): Double = {
      math.toRadians(x).toDouble
    }
  }

  trait DoubleIsRealPow extends RealPow[Double]{
    override def sqrt(x: Double): Double = math.sqrt(x)
    override def pow(x: Double, y:Double): Double = math.pow(x,y)
  }

  trait DoubleIsField extends Field[Double]{
    override def times(x: Double, y: Double): Double = x * y
    override def inv(x: Double): Double = 1/x
    override val one: Double = 1D
    override def negate(x: Double): Double = -x
    override val zero: Double = 0D
    override def plus(x: Double, y: Double): Double = x + y
  }



  implicit object IntIsCommutativeGroup extends IntIsCommutativeGroup with Ordering.IntOrdering
  implicit object FloatIsField extends FloatIsField with FloatIsTrig with FloatIsRealPow with FloatIsConvertibleFromDouble with Ordering.FloatOrdering
  implicit object DoubleIsField extends DoubleIsField with DoubleIsTrig with DoubleIsRealPow with DoubleIsConvertibleFromDouble
    with Ordering.DoubleOrdering

  trait ComplexIsField extends Field[ComplexOverField[Real]]{
    override def times(x: ComplexOverField[Real], y: ComplexOverField[Real]): ComplexOverField[Real] = x * y
    override def inv(x: ComplexOverField[Real]): ComplexOverField[Real] = one / x
    override val one: ComplexOverField[Real] = ComplexOverField(1D,0D)
    override def negate(x: ComplexOverField[Real]): ComplexOverField[Real] = -x
    override val zero: ComplexOverField[Real] = ComplexOverField(0D,0D)
    override def plus(x: ComplexOverField[Real], y: ComplexOverField[Real]): ComplexOverField[Real] = x + y

  }


  abstract class Double3IsEuclideanSpaceOverDouble(implicit ev: Field[Double]) extends EuclideanSpaceOverField[(Double,Double,Double), Double]{
    override def dotProduct(a: (Double, Double, Double), b: (Double, Double, Double), basis: Arr[(Double, Double, Double)]): Double = {
      //TODO dim check ??? must be 3 !

      def dot(i:Int, k:Int): Double ={
        var res = ev.zero
        val b1 = basis(i)
        val b2 = basis(k)

        res = ev.plus(res, ev.times(b1._1, b2._1))
        res = ev.plus(res, ev.times(b1._2, b2._2))
        res = ev.plus(res, ev.times(b1._3, b2._3))

        res
      }

      a._1 * b._1 * dot(0,0) + a._1 * b._2 * dot(0,1) + a._1 * b._3 * dot(0,2) +
        a._2 * b._1 * dot(1,0) + a._2 * b._2 * dot(1,1) + a._2 * b._3 * dot(1,2) +
        a._3 * b._1 * dot(2,0) + a._3 * b._2 * dot(2,1) + a._3 * b._3 * dot(2,2)

    }

    override def plus(a: (Double, Double, Double), b: (Double, Double, Double)): (Double, Double, Double) = {
      (a._1 + b._1, a._2 + b._2, a._3 + b._3)
    }

    override def negate(a: (Double, Double, Double)): (Double, Double, Double) = {
      (-a._1, -a._2, -a._3)
    }

    override def times(a: (Double, Double, Double), k: Double): (Double, Double, Double) = {
      (a._1 * k, a._2 * k, a._3 * k)
    }

    override def zero: (Double, Double, Double) = {
      (0,0,0)
    }

    val canonicalBasis:Arr[(Double, Double, Double)] = Arr((1D,0,0), (0,1D,0), (0,0,1D))
  }

  implicit object Double3IsEuclideanSpaceOverDouble extends Double3IsEuclideanSpaceOverDouble

  override def toString: String = {
    "Double"
  }

}


object Math{
  def nextPowerOfTwo(n: Int): Int = {
    val x = java.lang.Integer.highestOneBit(n)
    if (x == n) n else x * 2
  }
}

/**
  * Created by russoul on 28.01.17.
  */
class Arr[@specialized T](var array:Array[T], var size:Int)(implicit val ct: ClassTag[T])
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



  final def contains(key: T): Boolean =
  {
    for(v <- this){
      if(v == key) return true
    }

    false
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
}

object Arr
{
  private final val DEFAULT_SIZE = 8

  /**
    * Allocate an empty Buffer.
    */
  def empty[@specialized T: ClassTag](): Arr[T] =
    ofSize[T](DEFAULT_SIZE)

  /**
    * Allocate an empty Buffer, capable of holding n items without
    * resizing itself.
    *
    * This method is useful if you know you'll be adding a large number
    * of elements in advance and you want to save a few resizes.
    */
  def ofSize[@specialized T: ClassTag](n: Int): Arr[T] =
    new Arr(new Array[T](Math.nextPowerOfTwo(n)), 0)

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

import TypeClasses._
import TypeClasses.{Field, RealPow}
import TypeClasses.Field.Implicits._

/**
  * Created by russoul on 20.05.17.
  */
case class ComplexOverField[A](real: A, imaginary:A)(implicit ev: Field[A], pow:RealPow[A]) {

  @inline def +(that:ComplexOverField[A]): ComplexOverField[A] =
  {
    ComplexOverField(real + that.real, imaginary + that.imaginary)
  }

  @inline def -(that:ComplexOverField[A]): ComplexOverField[A] =
  {
    ComplexOverField(real - that.real, imaginary - that.imaginary)
  }

  @inline def unary_-(): ComplexOverField[A] =
  {
    ComplexOverField(-real, -imaginary)
  }

  @inline def *(that:ComplexOverField[A]): ComplexOverField[A] =
  {
    ComplexOverField(real * that.real - imaginary * that.imaginary, real * that.imaginary + imaginary * that.real)
  }

  @inline def *(scalar: A): ComplexOverField[A] =
  {
    ComplexOverField(real * scalar, imaginary * scalar)
  }

  @inline def /(scalar: A): ComplexOverField[A] =
  {
    ComplexOverField(real / scalar, imaginary / scalar)
  }

  @inline  def squaredLength() : A = {
    real * real + imaginary * imaginary
  }

  @inline def length(): A ={
    pow.sqrt(squaredLength())
  }

  @inline def conjugate() : ComplexOverField[A] = {
    ComplexOverField(real, -imaginary)
  }

  @inline def /(that : ComplexOverField[A]): ComplexOverField[A] = {
    this * that / squaredLength()
  }



}

object ComplexOverField{


  @inline def root(n:Int, num:ComplexOverField[Float]) : Arr[ComplexOverField[Float]] = {
    val res = new Arr[ComplexOverField[Float]](new Array[ComplexOverField[Float]](n), 0)


    val r = num.length()
    val trig = num / r

    val phi = math.acos(trig.real).toFloat

    val rRooted = math.pow(r, 1/n.toFloat).toFloat
    for(i <- 0 until n){
      val newPhi = (phi + 2*math.Pi * i)/n.toFloat
      res += ComplexOverField(math.cos(newPhi).toFloat * rRooted, math.sin(newPhi).toFloat * rRooted)
    }

    res
  }
}



import TypeClasses._

object Test extends App {

  /*implicit class thing(val s:StringContext){
    def ff() = macro Macros.impl
  }

  def floaty(str:String): Float = macro Macros.implFloat
  //def fieldy[A](str:String): A = macro Macros.impl[A]

  //println(floaty(implicitly[FieldLike[Float]].fromDouble(2) + ""))
  val ev = implicitly[FieldLike[Double]]
  val tt = "1" + "2"
  val t:Double = ff"10D"
  println(t)*/

  type T = (Double,Double,Double)

  class dim(val n:Int) extends Annotation{

  }


  @unchecked def dimTest(a:Arr[Int]@dim(2)): Unit =
  {

  }

  def testDot(a1:T, a2:T)(implicit ev: TypeClasses.EuclideanSpaceOverField[T, Double]): Double =
  {
    ev.dotProduct(a1,a2, ev.canonicalBasis)
  }

  val x = (1D,2D,3D)
  val y = (-1D, -2D, -3D)

  println(testDot(x,y))

  class TT(a:Int, b: Int){
    if(a == b) throw new Exception("tt")
  }

  class TT2(a:Int, b: Int){
  }

  for(i <- 0 until 10000){
    val t = new TT(i,i+2)
    System.nanoTime()

  }




  for(i <- 0 until 10){
    var t1 = System.nanoTime()
    for(i <- 0 until 100000){
      new TT2(i,i+1)
    }

    var t2 = System.nanoTime()
    println(t2 - t1)

    t1 = System.nanoTime()
    for(i <- 0 until 100000){
      new TT(i,i+1)
    }

    t2 = System.nanoTime()
    println(t2 - t1)
  }
}

package russoul.lib.common.math.algebra.deprecated;

/**
  * Created by russoul on 01.06.2017.
  */

//TODO redundant, do not use this class, use array instead
@immutable class Vec[@specialized(Float,Double,Int) F : ClassTag](private val array:Array[F], var isColumn:Boolean = true)(implicit ev: Field[F]){


  @inline @straight def x: F = array(0)
  @inline @straight def y: F = array(1)
  @inline @straight def z: F = array(2)
  @inline @straight def w: F = array(3)

  @inline def dim():Int = array.size
  @inline def size():Int = array.size

  @inline def foreach[U](f: (F => U)): Unit ={
    array.foreach(f)
  }

  @inline def filter[U](f: (F => Boolean)): Vec[F] ={
    new Vec(array.filter(f), isColumn)
  }

  @inline def min[B >: F]()(implicit ordering : Ordering[B]): F ={
    array.min(ordering)
  }

  @inline def setAsColumn(): Vec[F] = {
    isColumn = true
    this
  }

  @inline def setAsRow(): Vec[F] = {
    isColumn = false
    this
  }

  @inline @straight def apply(index: Int): F = {
    array(index-1)
  }

  @inline def *(scalar: F): Vec[F] = {
    val ar = new Array[F](array.size)
    for(i <- array.indices){
      ar(i) = array(i) * scalar
    }

    new Vec[F](ar)
  }

  @inline def /(scalar: F): Vec[F] = {
    val ar = new Array[F](array.size)
    for(i <- array.indices){
      ar(i) = array(i) / scalar
    }

    new Vec[F](ar)
  }


  @inline @straight def +(that: Vec[F]):Vec[F] = {
    assert(this.size() == that.size())
    val ar = new Array[F](array.size)
    for(i <- array.indices){
      ar(i) = this.array(i) + that.array(i)
    }


    new Vec[F](ar)
  }

  @inline @straight def -(that: Vec[F]):Vec[F] = {
    assert(this.size() == that.size())
    val ar = new Array[F](array.size)
    for(i <- array.indices){
      ar(i) = this.array(i) - that.array(i)
    }

    new Vec[F](ar)
  }

  @inline def ??+(that: Vec[F]): Boolean = {
    this.array.size == that.array.size
  }

  @inline def ??-(that: Vec[F]): Boolean = {
    this.array.size == that.array.size
  }

  @inline def unary_-():Vec[F] = {
    val ar = new Array[F](array.size)
    for(i <- 0 until array.size){
      ar(i) = -this.array(i)
    }

    new Vec[F](ar)
  }

  @inline def ??⨯(that:Mat[F]): Boolean ={
    if(isColumn) that.rows == 1 else that.rows == dim
  }

  @inline @straight def ⨯(that:Mat[F]): Mat[F] ={
    val p = that.columns

    if(isColumn){
      assert(1 == that.rows)
      val ar = new Array[F](dim * p)
      val res = new Mat[F](dim, p, ar)

      for(i <- 1 to dim){
        for(j <- 1 to p){
          res(i,j) = res(i,j) + this(i) * that(1,j)
        }
      }

      res
    }else{
      assert(size() == that.rows)
      val ar = new Array[F](1 * p)
      val res = new Mat[F](1, p, ar)

      for(j <- 1 to p){
        for(k <- 1 to dim) {
          res(1,j) = res(1,j) + this(k) * that(k,j)
        }
      }

      res
    }
  }

}

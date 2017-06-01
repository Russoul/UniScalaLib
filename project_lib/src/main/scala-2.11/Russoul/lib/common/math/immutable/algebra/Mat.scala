package Russoul.lib.common.math.immutable.algebra

import Russoul.lib.common.lang.mutable
import Russoul.lib.common.math.TypeClasses.Field
import Russoul.lib.common.utils.Arr

import Russoul.lib.common.math.TypeClasses.Field.Implicits._

/**
  * Created by russoul on 01.06.2017.
  */
@mutable case class Mat[F](val rows:Int,val columns:Int,val ar: Arr[F])(implicit ev: Field[F]) {

  @inline @unchecked def apply(i:Int)(j:Int): F ={
    ar( (i-1)*columns + (j-1) )
  }

  @inline @unchecked def +(that:Mat[F]): Mat[F] ={
    val ar = new Arr[F](rows * columns)

    for(i <- 0 until rows * columns){
      ar(i) = this.ar(i) + that.ar(i)
    }

    new Mat[F](rows, columns, ar)
  }

  @inline @unchecked def -(that:Mat[F]): Mat[F] ={
    val ar = new Arr[F](rows * columns)

    for(i <- 0 until rows * columns){
      ar(i) = this.ar(i) + that.ar(i)
    }

    new Mat[F](rows, columns, ar)
  }

  @inline @unchecked def *(scalar:F): Mat[F] ={
    val ar = new Arr[F](rows * columns)

    for(i <- 0 until rows * columns){
      ar(i) = this.ar(i) * scalar
    }

    new Mat[F](rows, columns, ar)
  }

  @inline @unchecked def /(scalar:F): Mat[F] ={
    val ar = new Arr[F](rows * columns)

    for(i <- 0 until rows * columns){
      ar(i) = this.ar(i) / scalar
    }

    new Mat[F](rows, columns, ar)
  }

  @inline @unchecked def ⨯(that:Mat[F]): Mat[F] ={
    val ar = new Arr[F](this.rows * that.columns)
    val res = new Mat[F](this.rows, that.columns, ar)

    for(i <- 1 to this.rows){
      for(j <- 1 to that.columns){
        for(k <- 1 to this.columns){ //== that.rows
          res(i)(j) += this(i)(k) * that(k)(j)
        }
      }
    }

    res
  }

  @inline @unchecked def ⨯(that:Vec[F]): Mat[F] ={

    val p = this.rows

    if(that.isColumn){
      val ar = new Arr[F](p * 1)
      val res = new Mat[F](p,1, ar)

      for(i <- 1 to p){
        for(k <- 1 to this.columns){
          res(i)(1) += this(i)(k) * that(k)
        }
      }

      res
    }else{
      val ar = new Arr[F](p * that.dim)
      val res = new Mat[F](p,that.dim, ar)

      for(i <- 1 to p){
        for(j <- 1 to that.dim){
          res(i)(j) += this(i)(1) * that(j)
        }
      }

      res
    }

  }


  @inline def isColumn():Boolean = columns == 1
  @inline def isRow(): Boolean = rows == 1
  @inline def isScalar():Boolean = isRow() && isColumn()

  @inline @unchecked def toRow(): Vec[F] =
  {
    val ar = new Arr[F](columns)
    for(i <- 1 to columns){
      ar(i) = this(1)(i)
    }

    Vec(ar).setAsRow()
  }

  @inline @unchecked def toColumn(): Vec[F] =
  {
    val ar = new Arr[F](rows)
    for(i <- 1 to rows){
      ar(i) = this(i)(1)
    }

    Vec(ar).setAsColumn()
  }

  @inline @unchecked def toScalar(): F =
  {
    this(1)(1)
  }

  @inline def ??⨯(that:Mat[F]): Boolean ={
    this.columns == that.rows
  }

  @inline def ??*(that:Mat[F]): Boolean = ??⨯(that)
  @inline @unchecked def *(that:Mat[F]): Mat[F] = ⨯(that)

}
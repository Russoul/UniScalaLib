package Russoul.lib.common.math.algebra.deprecated;

import scala.inline;
import scala.specialized;

/**
  * Created by russoul on 01.06.2017.
  */

@mutable class Mat[@specialized F : ClassTag](val rows:Int, val columns:Int, val ar: Array[F])(implicit ev: Field[F]) {

//Double definition error
  /*@inline @straight def apply(i:Int)(j:Int): F ={
    ar( (i-1)*columns + (j-1) )
  }*/

@inline @straight def apply(i:Int,j:Int): F ={
        ar( (i-1)*columns + (j-1) )
        }

@inline @straight def update(i: Int,j:Int,v: F): Unit = {
        ar( (i-1)*columns + (j-1) ) = v
        }


@inline @straight def +(that:Mat[F]): Mat[F] ={
        val ar = new Array[F](rows * columns)

        for(i <- 0 until rows * columns){
        ar(i) = this.ar(i) + that.ar(i)
        }

        new Mat[F](rows, columns, ar)
        }

@inline @straight def -(that:Mat[F]): Mat[F] ={
        val ar = new Array[F](rows * columns)

        for(i <- 0 until rows * columns){
        ar(i) = this.ar(i) + that.ar(i)
        }

        new Mat[F](rows, columns, ar)
        }

@inline @straight def *(scalar:F): Mat[F] ={
        val ar = new Array[F](rows * columns)

        for(i <- 0 until rows * columns){
        ar(i) = this.ar(i) * scalar
        }

        new Mat[F](rows, columns, ar)
        }

@inline @straight def /(scalar:F): Mat[F] ={
        val ar = new Array[F](rows * columns)

        for(i <- 0 until rows * columns){
        ar(i) = this.ar(i) / scalar
        }

        new Mat[F](rows, columns, ar)
        }

@inline @straight def ⨯(that:Mat[F]): Mat[F] ={
        val ar = new Array[F](this.rows * that.columns)
        val res = new Mat[F](this.rows, that.columns, ar)

        for(i <- 1 to this.rows){
        for(j <- 1 to that.columns){
        for(k <- 1 to this.columns){ //== that.rows
        res(i,j) = res(i,j) + this(i,k) * that(k,j)
        }
        }
        }

        res
        }

@inline @straight def ⨯(that:Vec[F]): Mat[F] ={

        val p = this.rows

        if(that.isColumn){
        val ar = new Array[F](p * 1)
        val res = new Mat[F](p,1, ar)

        for(i <- 1 to p){
        for(k <- 1 to this.columns){
        res(i,1) = res(i,1) + this(i,k) * that(k)
        }
        }

        res
        }else{
        val ar = new Array[F](p * that.dim)
        val res = new Mat[F](p,that.dim, ar)

        for(i <- 1 to p){
        for(j <- 1 to that.dim){
        res(i,j) = res(i,j) + this(i,1) * that(j)
        }
        }

        res
        }

        }


@inline def isColumn():Boolean = columns == 1
@inline def isRow(): Boolean = rows == 1
@inline def isScalar():Boolean = isRow() && isColumn()

@inline @straight def toRow(): Vec[F] =
        {
        val ar = new Array[F](columns)
        for(i <- 1 to columns){
        ar(i) = this(1,i)
        }

        Vec[F](ar).setAsRow()
        }

@inline @straight def toColumn(): Vec[F] =
        {
        val ar = new Array[F](rows)
        for(i <- 1 to rows){
        ar(i) = this(i,1)
        }

        Vec[F](ar).setAsColumn()
        }

@inline @straight def toScalar(): F =
        {
        this(1,1)
        }

@inline def ??⨯(that:Mat[F]): Boolean ={
        this.columns == that.rows
        }

@inline def ??*(that:Mat[F]): Boolean = ??⨯(that)
@inline @straight def *(that:Mat[F]): Mat[F] = ⨯(that)

        def genArray(): Array[F] = {
        val re = new Array[F](16)
        for(i <- 0 until 16) re(i) = ar(i)

        re
        }

        }

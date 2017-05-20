package Russoul.lib.common.math.immutable.algebra

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.utils.Vector

import scala.util.Sorting

/**
  * Created by russoul on 07.05.17.
  */
@immutable case class Interval private(min:Float, max:Float, empty:Boolean) extends Ordered[Interval]
{

  def &(i:Interval): Interval =
  {
    if(i.empty || this.empty) return Interval.empty()
    if(this.max < i.min || i.max < this.min) Interval.empty()
    else{
      val rmin = math.max(min, i.min)
      val rmax = math.min(max, i.max)

      Interval(rmin, rmax, false)
    }
  }

  def &(ii:Vector[Interval]) : Vector[Interval] =
  {
    val res = Vector.ofSize[Interval](ii.size)

    for(i <- ii){
      val t = i & this
      if(!t.empty) res += t
    }

    res
  }

  //TODO WORKS ONLY FOR NOT INTERSECTING INTERVALS
  def |(i:Interval):Vector[Interval] =
  {

    if(i.empty) return Vector(this)
    if(this.empty) return Vector(i)


    val maxOfMin = math.max(min, i.min)
    val minOfMax = math.min(max, i.max)


    if(maxOfMin > minOfMax){ //they do not intersect
      Vector(this, i)
    }else{ //they intersect
      val rmin = math.min(min, i.min)
      val rmax = math.max(max, i.max)

      Vector(Interval(rmin, rmax))
    }

  }


  def |(ii:Vector[Interval]):Vector[Interval] ={
    val i = Vector.ofSize[Interval](ii.size)//does not contain any empty intervals

    for(k <- ii){
      if(!k.empty) i += k
    }

    if(i.size == 0){
      Vector(this)
    }else{
      if(this.empty) return i

      val ret = Vector.ofSize[Interval](ii.size)

      var cur:Interval = this

      for(k <- i){
        if(k.max <= cur.min){
          ret += k
        }else if(k.min >= cur.max){
          ret += k
        }else{ //they intersect
          cur = this.copy(min = math.min(cur.min, k.min), max = math.max(cur.max, k.max))
        }
      }

      ret += cur

      ret

    }

  }


  override def compare(that: Interval): Int = {
    if(this.empty){
      if(that.empty){
        0
      }else{
        -1
      }
    }else{
      if(that.empty){
        1
      }else{
        if(this.max > that.max) 1
        else if(this.max < that.max) -1
        else {
          if(this.min > that.min) 1
          else if(this.min < that.min) -1
          else 0
        }
      }
    }
  }


  override def toString(): String =
  {
    if(empty){
      "Interval[EMPTY]"
    }else{
      "Interval[ " + min + " ; " + max + " ]"
    }
  }




}

object Interval extends Ordering[Interval]{


  override def compare(x: Interval, y: Interval): Int = {
    x compare y
  }

  implicit class impl(chain:Vector[Interval]){

    def |(ii:Vector[Interval]): Vector[Interval] = {

      if(ii.size == 0) return chain

      if(ii.size == 1){
        ii(0) | chain
      }else{
        var curChain = ii(0) | chain

        for(i <- 1 until ii.size){
          curChain = ii(i) | curChain
        }

        curChain
      }
    }


    def &(ii:Vector[Interval]): Vector[Interval] = {
      if(ii.size == 1){
        ii(0) & chain
      }else{
        var curChain = Vector(Interval.empty())

        for(i <- 0 until ii.size){
          curChain |= ii(i) & chain
        }

        curChain
      }
    }


    def |(ii:Interval): Vector[Interval] = {
      ii | chain
    }

    def &(ii:Interval): Vector[Interval] = {
      ii & chain
    }

    def sort():Vector[Interval] = {
      chain.insertionSort(Interval)
    }

    override def toString(): String =
    {

      var res = "Interval[ "
      for(i <- chain){
        if(i.empty) res += "EMPTY ; "
        else res += ( "( " + i.min + " ; " + i.max + " ) " )
      }

      res += " ]"

      res
    }

  }

  implicit class impl2(i:Interval){

    def toVector(): Vector[Interval] =
    {
      Vector(i)
    }
  }

  def empty(): Interval =
  {
    new Interval(0,0,true)
  }

  def apply(min:Float, max:Float):Interval = {
    new Interval(min, max, false)
  }
}
package Russoul.lib.common.math.immutable.algebra

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.utils.vector

/**
  * Created by russoul on 07.05.17.
  */
@immutable case class Interval private(min:Float, max:Float, empty:Boolean)
{

  def &(i:Interval): Interval =
  {
    if(i.empty || this.empty) return Interval.empty()
    if(this.max < i.min || i.max < this.min) return Interval.empty()
    else{
      val rmin = math.max(min, i.min)
      val rmax = math.min(max, i.max)

      return new Interval(rmin, rmax, false)
    }
  }

  def &(ii:vector[Interval]) : vector[Interval] =
  {
    val res = vector.ofSize[Interval](ii.size)

    for(i <- ii){
      val t = i & this
      if(!t.empty) res += t
    }

    return res
  }

  //TODO WORKS ONLY FOR NOT INTERSECTING INTERVALS
  def |(i:Interval):vector[Interval] =
  {

    if(i.empty) return vector(this)
    if(this.empty) return vector(i)


    val maxOfMin = math.max(min, i.min)
    val minOfMax = math.min(max, i.max)


    if(maxOfMin > minOfMax){ //they do not intersect
      return vector(this, i)
    }else{ //they intersect
      val rmin = math.min(min, i.min)
      val rmax = math.max(max, i.max)

      return vector(Interval(rmin, rmax))
    }

  }


  def |(ii:vector[Interval]):vector[Interval] ={
    val i = vector.ofSize[Interval](ii.size)//does not contain any empty intervals

    for(k <- ii){
      if(!k.empty) i += k
    }

    if(i.size == 0){
      return vector(this)
    }else{
      if(this.empty) return i

      val ret = vector.ofSize[Interval](ii.size)

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

      return ret

    }

  }

  override def toString(): String =
  {
    if(empty){
      return "Interval[EMPTY]"
    }else{
      return "Interval[ " + min + " ; " + max + " ]"
    }
  }



}

object Interval{

  implicit class impl(chain:vector[Interval]){

    def |(ii:vector[Interval]): vector[Interval] = {

      if(ii.size == 0) return chain

      if(ii.size == 1){
        return ii(0) | chain
      }else{
        var curChain = ii(0) | chain

        for(i <- 1 until ii.size){
          curChain = ii(i) | curChain
        }

        return curChain
      }
    }


    def &(ii:vector[Interval]): vector[Interval] = {
      if(ii.size == 1){
        return ii(0) & chain
      }else{
        var curChain = vector(Interval.empty())

        for(i <- 0 until ii.size){
          curChain |= ii(i) & chain
        }

        return curChain
      }
    }


    def |(ii:Interval): vector[Interval] = {
      return ii | chain
    }

    def &(ii:Interval): vector[Interval] = {
      return ii & chain
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
    def toVector(): vector[Interval] =
    {
      vector(i)
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
package russoul.lib.common.utils

import scala.collection.mutable

/**
  * Created by Russoul on 19.07.2016.
  */
class Timer
{
  private val table = new mutable.HashMap[String, Double]()


  def update(key:String): Unit =
  {
    table(key) = Timer.getTimeNano()
  }

  def hasKey(key:String): Boolean =
  {
    table.get(key).nonEmpty
  }

  def remove(key:String): Option[Double] =
  {
    table.remove(key)
  }


  /**
    *
    * @param key
    * @return in nanoseconds
    */
  def getDelta(key:String):Double =
  {
    val time = table.get(key)
    if(time.nonEmpty){
      Timer.getTimeNano() - time.get
    }else{
      -1D
    }
  }

  def getDeltaNano(key:String):Double =
  {
    getDelta(key)
  }

  def getDeltaMilli(key:String):Double =
  {
    getDeltaNano(key)/1000000D
  }


  def getDeltaMicro(key:String):Double =
  {
    getDeltaNano(key)/1000D
  }

  def getDeltaSec(key:String):Double =
  {
    getDeltaNano(key)/1000000000D
  }

  /**
    *
    * @param key
    * @return in nanoseconds
    */
  def getLastUpdateTime(key:String):Double =
  {
    val got = table.get(key)
    if(got.isDefined){
      got.get
    }else{
      -1D
    }
  }




}

object Timer
{

  def getTimeNano(): Double =
  {
    System.nanoTime().toDouble
  }

  def getTimeMilli():Double =
  {
    System.nanoTime().toDouble / 1000000
  }

  /**
    *
    * @param howToRender string the will be printed given delta time of provided operation in milliseconds
    * @param f operation to be timed (lazy input)
    * @tparam T
    * @return result of operation
    */
  def timed[T](howToRender : Double => String)(f: => T):T =
  {
    val t1 = getTimeMilli()
    val res = f
    val t2 = getTimeMilli()
    println(howToRender(t2-t1))
    res
  }
}

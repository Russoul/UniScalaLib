package Russoul.lib.common.utils

/**
  * Created by Russoul on 19.07.2016.
  */
class Timer
{
  private var prevFrameTime = 0D
  private var prevInputTime = 0D
  private var prevSystemTime = 0D

  /**
    * @note InternalUse
    */
  def onFrameUpdate(): Unit =
  {
    prevFrameTime = Timer.getTimeNano()
  }

  def onSystemUpdate():Unit =
  {
    prevSystemTime = Timer.getTimeNano()
  }

  def onInputUpdate(): Unit =
  {
    prevInputTime = Timer.getTimeNano()
  }

  def getFrameDelta() = Timer.getTimeNano() - prevFrameTime

  def getInputDelta() = Timer.getTimeNano() - prevInputTime

  def getSystemDelta() = Timer.getTimeNano() - prevSystemTime

  def init(): Unit =
  {
    prevFrameTime = Timer.getTimeNano()
    prevInputTime = Timer.getTimeNano()
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

  def timed[T](str:String)(f: => T):T =
  {
    val t1 = getTimeMilli()
    val res = f
    val t2 = getTimeMilli()
    println(str +": "+(t2-t1))
    res
  }
}

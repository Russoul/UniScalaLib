package Russoul.lib.context.scene.structure.sample

import scala.language.postfixOps

/**
  * Created by Russoul on 18.07.2016.
  */
class VoxelData(val density:Array[Float], val color:Int)
{
  private def copyArray():Array[Float] =
  {
    val newArray = new Array[Float](8)
    for(i <- 0 until 8 ) newArray(i) = density(i)

    newArray
  }


  def copy() = new VoxelData(copyArray(), color)

  def containsSurface(isoLevel:Float):Boolean =
  {
    var t = 0
    for(i <- 0 until 8 ){
      if(density(i) < isoLevel) t+=1
    }

    if(t == 8 || t == 0) false else true
  }

  def nonEmpty(isoLevel:Float): Boolean =
  {
    var t = 0
    for(i <- 0 until 8 ){
      if(density(i) < isoLevel) t+=1
    }

    t != 0
  }
}

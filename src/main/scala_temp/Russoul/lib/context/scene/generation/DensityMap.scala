package Russoul.lib.context.scene.generation

import libnoiseforjava.module.ModuleBase

/**
  * Created by Russoul on 28.07.2016.
  */
class DensityMap(noiseGen:ModuleBase, sizeX:Int, sizeY:Int, sizeZ:Int)
{
  val array = Array.ofDim[Float](sizeX, sizeY, sizeZ)


  def build(inputMinX:Float, inputMaxX:Float, inputMinY:Float, inputMaxY:Float, inputMinZ:Float, inputMaxZ:Float): DensityMap ={
    val dX = inputMaxX - inputMinX
    val dsX = dX/sizeX

    val dY = inputMaxY - inputMinY
    val dsY = dY/sizeY

    val dZ = inputMaxZ - inputMinZ
    val dsZ = dZ/sizeZ

    for(x <- array.indices){
      for(y <- array(0).indices){
        for(z <- array(0)(0).indices){
          array(x)(y)(z) = noiseGen.getValue(inputMaxX + dsX/2 + dsX * x, inputMaxY + dsY/2 + dsY * y, inputMaxZ + dsZ/2 + dsZ * z).toFloat
        }
      }
    }

    this
  }

  def get(x:Int,y:Int,z:Int) = array(x)(y)(z)
}

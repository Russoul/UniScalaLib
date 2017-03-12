package Russoul.lib.context.scene.generation

import java.io.File

import Russoul.lib.common.utils.ImageUtils
import libnoiseforjava.module.ModuleBase
import libnoiseforjava.util._

/**
  * Created by Russoul on 28.07.2016.
  */
class HeightMap(noiseGen:ModuleBase, sizeX:Int, sizeZ:Int)
{
  private val heightMap = new NoiseMap(sizeX, sizeZ)
  private val heightMapBuilder = new NoiseMapBuilderPlane

  heightMapBuilder.setSourceModule (noiseGen)
  heightMapBuilder.setDestNoiseMap (heightMap)
  heightMapBuilder.setDestSize (sizeX, sizeZ)

  private val renderer = new RendererImage
  private val image = new ImageCafe(sizeX, sizeZ)
  renderer.setSourceNoiseMap(heightMap)
  renderer.setDestImage(image)


  def build(inputMinX:Float, inputMaxX:Float, inputMinZ:Float, inputMaxZ:Float):HeightMap = {
    heightMapBuilder.setBounds(inputMinX, inputMaxX, inputMinZ, inputMaxZ)
    heightMapBuilder.build()

    this
  }
  
  def defaultColorize():HeightMap = 
  {
    renderer.clearGradient()
    renderer.addGradientPoint (-1.0000, new ColorCafe (  86,86,86, 255)); // stone
    renderer.addGradientPoint (-0.2500, new ColorCafe(  32, 160,   0, 255)); //plain
    renderer.addGradientPoint ( 0.0000, new ColorCafe (  86,86,86, 255)); // shore
    renderer.addGradientPoint ( 0.0625, new ColorCafe (240, 240,  64, 255)); // sand
    renderer.addGradientPoint ( 0.1250, new ColorCafe ( 32, 160,   0, 255)); // grass
    renderer.addGradientPoint ( 0.3750, new ColorCafe (224, 224,   0, 255)); // dirt
    renderer.addGradientPoint ( 0.7500, new ColorCafe (128, 128, 128, 255)); // rock
    renderer.addGradientPoint ( 0.9000, new ColorCafe (255, 255, 255, 255)); // snow

    this
  }

  def render() = {
    renderer.render()

    this
  }

  def getColor(x:Int, z:Int) = image.getValue(x,z)

  def get(x:Int,z:Int):Float = heightMap.getValue(x,z).toFloat

  def getArray() = heightMap.noiseMap




}

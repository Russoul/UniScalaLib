package Russoul.lib.common.utils

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO


object ImageUtils
{

  /**
    *
    * @param array rgba input -> rgb output
    * @param width
    * @param height
    * @param filepath
    */
  def createRGBImage(array:Array[Byte], width:Int, height:Int, filepath:String) =
  {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for(i <- 0 until height){
      for(j <- 0 until width){
        val r = array(0 + 4*j+ 4*width*i)
        val g = array(1 + 4*j+ 4*width*i)
        val b = array(2 + 4*j+ 4*width*i)

        image.setRGB(j, i, ColorUtils.genRGB(r,g,b))
      }
    }

    val f = new File(filepath)
    if (!f.exists()) f.createNewFile()
    ImageIO.write(image, "PNG", f)
  }

  def createRGBImageOfNoise(array: Array[Array[Int]], filePath: String) =
  {
    val image = new BufferedImage(array.length, array(0).length, BufferedImage.TYPE_INT_RGB)
    for (i <- array.indices) {
      for (j <- array(0).indices) {
        val in = array(i)(j)
        image.setRGB(i, j, in)
      }
    }

    val f = new File(filePath)
    if (!f.exists()) f.createNewFile()
    ImageIO.write(image, "PNG", f)
  }

  /*def createRGBImageOfNoise(img: ImageCafe, filePath: String) =
  {
    val image = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_RGB)
    for (i <- 0 until img.getWidth) {
      for (j <- 0 until img.getHeight) {
        val in = img.getValue(i,j)
        image.setRGB(i, j, ColorUtils.genRGB(in.getRed, in.getGreen, in.getBlue))
      }
    }

    val f = new File(filePath)
    if (!f.exists()) f.createNewFile()
    ImageIO.write(image, "PNG", f)
  }*/
}

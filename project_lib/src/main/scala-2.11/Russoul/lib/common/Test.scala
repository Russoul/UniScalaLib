package Russoul.lib.common

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import Russoul.lib.common.math.immutable.linear.vec3
import Russoul.lib.common.utils.ImageUtils
/**
  * Created by russoul on 01.04.17.
  */
object Test extends App
{

  import javax.imageio.ImageIO
  import java.awt.image.BufferedImage
  import java.io.ByteArrayInputStream
  import java.io.IOException

  def rawToPNG(bytes: Array[Byte], outputFile: FileOutputStream): Unit = {
    try {
      val img = ImageIO.read(new ByteArrayInputStream(bytes))
      ImageIO.write(img, "png", outputFile)
    } catch {
      case e: IOException =>

      // Handle exception
    }
  }


  val stream = new FileInputStream("/home/russoul/dev/proj/ScalaLibrary/project_lib/font.bdf")

  val info = ImageUtils.readBDF(stream)


  val A = info.getGlyph(info.getCode("A"))


  //rawToPNG(buf.array(), new FileOutputStream("test.png"))
  //ImageIO.write(img, "png", new FileOutputStream("test.png"))

  val im = info.genARGBBitmapBuffer(0x41 to 0x5A, vec3(0,0,0), directBuffer = true)
  val img = ImageUtils.toBufferedImage(im._1, im._2, im._3)
  ImageIO.write(img, "png", new File("test.png"))
}

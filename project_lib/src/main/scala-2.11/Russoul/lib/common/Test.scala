package Russoul.lib.common

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import Russoul.lib.common.math.{CollisionEngine, Math}
import Russoul.lib.common.math.immutable.geometry.simple.{Circle, Line2, Rectangle2}
import Russoul.lib.common.math.immutable.linear.{vec2, vec3}
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

 /* def rawToPNG(bytes: Array[Byte], outputFile: FileOutputStream): Unit = {
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
  ImageIO.write(img, "png", new File("test.png"))*/

  val l1 = new Line2(vec2(-2,0), vec2(1,3))
  val l2 = new Line2( vec2(1,0), vec2(-1,4))
  val l3 = new Line2(vec2(0,0), vec2(0,10))

  val rec = new Rectangle2(vec2(0,2), vec2(1,1))
  val circle = new Circle(vec2(2,-2), 1)

  val i = CollisionEngine.checkLine2Line2(l1, l2)
  val i2 = CollisionEngine.checkLine2Rectangle2Min(l1, rec)

  val i3 = CollisionEngine.distanceCircleLine(circle, l3)



  /*println(i.isDefined)

  if(i.isDefined){
    println(i.get)
  }

  println(i2.isDefined)
  if(i2.isDefined){
    println(i2.get._1)
  }*/

  println(i3._1)
}

package Russoul.lib.context.loader

import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.io.{File, FileNotFoundException}
import javax.imageio.ImageIO

import org.lwjgl.BufferUtils


object GLImageLoader
{
  private final val BYTES_PER_PIXEL = 4


  def loadImage(path: String): Int =
  {
    if (path == null) throw new NullPointerException

    val file = new File(path)

    if (!file.exists()) throw new FileNotFoundException(path)

    var img = ImageIO.read(file)

    val locationX = img.getWidth() / 2
    val locationY = img.getHeight() / 2


    val tx = AffineTransform.getRotateInstance(Math.toRadians(0), locationX, locationY)
    val op = new AffineTransformOp(tx, AffineTransformOp.TYPE_BILINEAR)
    img = op.filter(img, null)

    val pixels = new Array[Int](img.getWidth * img.getHeight)


    img.getRGB(0, 0, img.getWidth, img.getHeight, pixels, 0, img.getWidth)

    val buffer = BufferUtils.createByteBuffer(img.getWidth * img.getHeight * BYTES_PER_PIXEL)

    for (y <- 0 until img.getHeight()) {
      for (x <- 0 until img.getWidth()) {
        val pixel = pixels(y * img.getWidth() + x)
        buffer.put(((pixel >> 16) & 0xFF).toByte); // Red component
        buffer.put(((pixel >> 8) & 0xFF).toByte); // Green component
        buffer.put((pixel & 0xFF).toByte); // Blue component
        buffer.put(((pixel >> 24) & 0xFF).toByte); // Alpha component. Only for RGBA
      }
    }
    buffer.flip()

    import org.lwjgl.opengl.GL11._


    val textureID = glGenTextures(); //Generate texture ID
    glBindTexture(GL_TEXTURE_2D, textureID); //Bind texture ID

    //Setup wrap mode
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT) //GL12.GL_CLAMP_TO_EDGE
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)

    //Setup texture scaling filtering
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)

    //Send texel data to OpenGL
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, img.getWidth(), img.getHeight(), 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer)

    //Return the texture ID so we can bind it later again
    textureID
  }
}

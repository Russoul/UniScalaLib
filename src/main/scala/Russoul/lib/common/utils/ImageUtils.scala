package Russoul.lib.common.utils

import java.awt.image.BufferedImage
import java.io._
import java.nio.ByteBuffer
import java.util.regex.{Matcher, Pattern}
import scala.collection.mutable
import Russoul.lib.common.Real
import Russoul.lib.common.math.algebra.Vec3


/**
  * Created by russoul on 30.03.17.
  */
object ImageUtils
{

  private class BDFReadException(str:String) extends Exception(str){}


  private class BitmapException(str:String) extends Exception(str){}


  /*object BitmapBitsPerPixel extends Enumeration
  {
    type BitmapBitsPerPixel = Value
    val bit, byte, short, int, long = Value
  }*/

  /**
    *
    * @param byteBuffer heap buffer !!!
    * @param bitsPerRow must be a multiple of 8, or else it could not be implemented cause access to individual bits is not allowed on JVM (or on almost any other machine)
    */
  class Bitmap(val byteBuffer:ByteBuffer,private val height:Int, private val bitsPerRow:Int)
  {
    def this(height:Int,bitsPerRow:Int){
      this(ByteBuffer.allocate(bitsPerRow/8 * height), height, bitsPerRow)
      //TODO maybe store bytesPerRow not bits then the exception will not be needed
      if(bitsPerRow % 8 != 0) throw new BitmapException("Bits per row must be a multiple of 8 !")
    }


    def getWidth() = bitsPerRow
    def getHeight() = height


    def size(): Int =
    {
      bitsPerRow/8 * height
    }

    def setByte(x:Int, y:Int, data:Byte): Unit =
    {
      byteBuffer.put(y*bitsPerRow/8 + x, data)
    }

    def setInt(x:Int, y:Int, data:Int): Unit =
    {
      byteBuffer.putInt(y*bitsPerRow/8 + x, data)
    }

    def rowToHexString(y:Int): String =
    {
      var str:String = ""

      for(x <- 0 until bitsPerRow/8){
        str += ByteUtils.bytesToHex(Array[Byte](byteBuffer.get(y*bitsPerRow/8 + x)))
      }

      str
    }

    def toHexString(): String =
    {
      var str:String = ""
      for(y <- 0 until height){
        str += rowToHexString(y) + "\n"
      }

      str
    }

    def toRGBABitmapBuffer(color:Vec3[Real], direct:Boolean): ByteBuffer =
    {
      val size = bitsPerRow * height * 4
      val buffer = if(!direct) ByteBuffer.allocate(size) else ByteBuffer.allocateDirect(size)
      for(y <- 0 until height){
        for(x <- 0 until bitsPerRow/8){
          val byte = this.byteBuffer.get(y*bitsPerRow/8 + x)
          for(i <- 0 until 8){
            val bit = ((byte & 0xFF) >> (7-i)) & 1
            buffer.put(y*bitsPerRow*4+x*4*8 + 4*i + 0, (color.x * 255 * bit).toByte)
            buffer.put(y*bitsPerRow*4+x*4*8 + 4*i + 1, (color.y * 255 * bit).toByte)
            buffer.put(y*bitsPerRow*4+x*4*8 + 4*i + 2, (color.z * 255 * bit).toByte)
            buffer.put(y*bitsPerRow*4+x*4*8 + 4*i + 3, (255 * bit).toByte)
          }
        }
      }

      buffer
    }


    def toARGBBitmapBuffer(color:Vec3[Real], direct:Boolean): ByteBuffer =
    {
      val size = bitsPerRow * height * 4
      val buffer = if(!direct) ByteBuffer.allocate(size) else ByteBuffer.allocateDirect(size)
      for(y <- 0 until height){
        for(x <- 0 until bitsPerRow/8){
          val byte:Byte = this.byteBuffer.get(y*bitsPerRow/8 + x)

          for(i <- 0 until 8){

            val bit:Int = ((byte & 0xFF) >> (7-i)) & 1

            buffer.put(y*bitsPerRow*4   +x*4*8 + 4*i + 1, (color.x * 255 * bit).toByte)
            buffer.put(y*bitsPerRow*4   +x*4*8 + 4*i + 2, (color.y * 255 * bit).toByte)
            buffer.put(y*bitsPerRow*4   +x*4*8 + 4*i + 3, (color.z * 255 * bit).toByte)
            buffer.put(y*bitsPerRow*4   +x*4*8 + 4*i + 0, (255 * bit).toByte)
          }
        }
      }



      buffer
    }

  }


  /**
    *
    * @param argbRaw ARGB format input !!!
    * @return
    */
  def toBufferedImage(argbRaw: ByteBuffer, width:Int, height:Int): BufferedImage = {

    val intBuffer = argbRaw.asIntBuffer()
    val i = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    var y = 0
    while ( {
      y < height
    }) {
      var x = 0
      while ( {
        x < width
      }) {
        val argb = intBuffer.get(width*y+x)
        i.setRGB(x, y, argb)

        {
          x += 1; x
        }
      }

      {
        y += 1; y
      }
    }
    i
  }

  /**
    *
    * @param name
    * @param encoding
    * @param width this is real width in pixels, the width of the bitmap maybe be higher (next multiple of 8)
    * @param height the height is always the same for real pixels and bitmap size
    * @param offsetX offsets count from bottom left corner of the glyph
    * @param offsetY offset of the boundingbox of the glyph
    * @param nextX offset in pixels to the next glyph
    * @param nextY
    * @param bitmap 1 bit per pixel !!! just on and off (this causes its width to be possibly higher than pixel count on a row,
    *               machine can only access bytes at a time not bits, so bitmap width is extended to be a multiple of 8)
    *
    *               GLYPH is a raster representation of a font (at a particular resolution so its not meant for scaling)
    *               GLYPH bitmap is usually generated from vector font representation, say TRUETYPE font
    */
  class Glyph(val name:String, val encoding:Int, val width:Int, val height:Int,
              val offsetX:Int, val offsetY:Int, val nextX:Int, val nextY:Int, val bitmap:Bitmap)
  {

  }

  //BDF (font)
  class BDFInfo(read:BufferedReader)
  {

    private var glyphCount:Int = _
    private val glyphs     = new mutable.HashMap[Int, Glyph]()
    private val glyphNames = new mutable.HashMap[String,Int]()

    {
      readAll()
      read.close()
    }

    def getGlyphCount() = glyphCount

    def getGlyph(unicode:Int) = glyphs(unicode)
    def hasGlyph(unicode:Int) = glyphs.contains(unicode)
    def getAllNames(): Iterable[String] = glyphNames.keys //this makes glyphNames immutable on caller side (doesn't give the caller the actual object but its copy)
    def getAllCodes(): Iterable[Int] = glyphNames.values
    def getCode(name:String) = glyphNames(name)
    def hasCode(name:String) = glyphNames.contains(name)


    /**
      *
      * @param range
      * @param color
      * @return (data, width, height)
      */
    def genARGBBitmapBuffer(range:Range, color:Vec3[Real], directBuffer:Boolean): (ByteBuffer, Int, Int) =
    {
      var width:Int = 0
      var height:Int = 0

      for(i <- range){
        val glyph = getGlyph(i)
        height += glyph.bitmap.getHeight()
        if(glyph.bitmap.getWidth() > width) width = glyph.bitmap.getWidth()
      }


      val data = if(!directBuffer) ByteBuffer.allocate(width * height * 4) else ByteBuffer.allocateDirect(width * height * 4)
      var curHeight = 0


      for(i <- range){
        val glyph = getGlyph(i)
        val buf = glyph.bitmap.toARGBBitmapBuffer(color, directBuffer)
        val levelSize = glyph.bitmap.getWidth()*4

        for(j <- 0 until glyph.bitmap.getHeight()){
          val offset = levelSize*j

          for(k <- 0 until levelSize){
            data.put(buf.get(offset + k))
          }
          data.position(curHeight*width*4)

          //System.arraycopy(buf.array(), offset, data.array(), curHeight*4*width,levelSize)// wont work for direct buffers
          curHeight += 1

        }
      }

      data.position(0)
      (data,width,height)

    }

    /**
      *
      * @param range
      * @param color
      * @return (data, width, height)
      */
    def genRGBABitmapBuffer(range:Range, color:Vec3[Real], directBuffer:Boolean): (ByteBuffer, Int, Int) =
    {
      var width:Int = 0
      var height:Int = 0

      for(i <- range){
        val glyph = getGlyph(i)
        height += glyph.bitmap.getHeight()
        if(glyph.bitmap.getWidth() > width) width = glyph.bitmap.getWidth()
      }


      val data = if(!directBuffer) ByteBuffer.allocate(width * height * 4) else ByteBuffer.allocateDirect(width * height * 4)
      var curHeight = 0


      for(i <- range){
        val glyph = getGlyph(i)
        val buf = glyph.bitmap.toRGBABitmapBuffer(color, directBuffer)
        val levelSize = glyph.bitmap.getWidth()*4

        for(j <- 0 until glyph.bitmap.getHeight()){
          val offset = levelSize*j

          for(k <- 0 until levelSize){
            data.put(buf.get(offset + k))
          }
          data.position(curHeight*width*4)

          //System.arraycopy(buf.array(), offset, data.array(), curHeight*4*width,levelSize)// wont work for direct buffers
          curHeight += 1

        }
      }

      data.position(0)
      (data,width,height)

    }

    private def extract(regex:String): Option[Matcher] =
    {
      var line:String = null

      val pat = Pattern.compile(regex)

      while({line = read.readLine();line != null}){
        if(line.matches(regex)){
          return Some(pat.matcher(line))
        }
      }

      None
    }

    private def readAll(): Unit =
    {

      val EXCEPTION_TEXT = "Can not read BDF font, some data is missing or file is not correct !"

      var line:String = null

      val regexDeviceWidth = Pattern.compile("^DWIDTH ([+-]?\\d+) ([+-]?\\d+)$")
      val regexNewChar = Pattern.compile("^STARTCHAR (.+)$")
      val regexEndChar = Pattern.compile("^ENDCHAR$")
      val regexEncoding = Pattern.compile("^ENCODING (\\d+)$")
      val regexDimensions = Pattern.compile("^BBX ([+-]?\\d+) ([+-]?\\d+) ([+-]?\\d+) ([+-]?\\d+)$")
      val regexBitmap = Pattern.compile("^BITMAP$")
      val regexCharCount = Pattern.compile("^CHARS (\\d+)$")


      var matcher:Matcher = null

      var curName:String = null
      var curEncoding, curWidth,curHeight, curOffsetX, curOffsetY, curNextX, curNextY:Int = -1 //placeholder for super big number
      var curBitmap:Bitmap = null

      while({line = read.readLine();line != null && line != "ENDFONT"}){

        //println(line)

        if({matcher = regexNewChar.matcher(line); matcher.matches()}){
          curName = matcher.group(1)

        }else if({matcher = regexEndChar.matcher(line); matcher.matches()}){
          if(curName != null && curWidth != -1 && curHeight != -1  && curBitmap != null){
            glyphs(curEncoding) = new Glyph(curName, curEncoding, curWidth, curHeight, curOffsetX, curOffsetY, curNextX, curNextY,  curBitmap)
            glyphNames(curName) = curEncoding

            curName = null
            curEncoding = -1
            curWidth    = -1
            curHeight   = -1
            curOffsetX  = -1
            curOffsetY  = -1
            curNextX    = -1
            curNextY    = -1
            curBitmap = null
          }else{
            throw new BDFReadException(EXCEPTION_TEXT)
          }
        }

        else if({matcher = regexEncoding.matcher(line); matcher.matches()}) {
          curEncoding = matcher.group(1).toInt

        }else if({matcher = regexDeviceWidth.matcher(line); matcher.matches()}){
          curNextX = matcher.group(1).toInt
          curNextY = matcher.group(2).toInt
        }else if({matcher = regexDimensions.matcher(line); matcher.matches()}){
          curWidth = matcher.group(1).toInt
          curHeight = matcher.group(2).toInt
          curOffsetX = matcher.group(3).toInt
          curOffsetY = matcher.group(4).toInt
        }else if({matcher = regexBitmap.matcher(line); matcher.matches()}){

          if(curWidth == -1 || curHeight == -1) throw new BDFReadException(EXCEPTION_TEXT)

          //var lines = 0//number of lines is equal to the height of the glyph
          val charCountInALineTemp = (curWidth/8) * 8 + (if(curWidth % 8 != 0) 8 else 0)//this return next multiple of 8 greater or eq to curWidth
          val charCountInALine = charCountInALineTemp/8 * 2 //number of chars in a line (a must !)

          curBitmap = new Bitmap(curHeight, charCountInALineTemp)


          for(lines <- 0 until curHeight){//curHeight equals to num of lines of bitmap
            line = read.readLine()
            if(line.size != charCountInALine){
              throw new BDFReadException("Line number " + (lines + 1) + " of bitmap image of " + curName + " char is incorrect, correct line length: " + charCountInALine + ", got: "+line.size + ", line: " + line + " !")
            }

            /*println("char count on a line: " + charCountInALine)
            println("line: " + lines)
            println("width: " + curWidth)
            println("height: " + curHeight)
            println("bitmap size: " + curBitmap.size())
            println("lineS: "+ line)
            println("name: " + curName)*/


            for(i <- 0 until charCountInALine/2){//always is a multiple of 2
              val byte:Int = Integer.parseInt(line.substring(2*i,2*i+2), 16) //interpret string as hex
              curBitmap.setByte(i, lines, byte.toByte)

              //println("i: " + i)
            }

          }
          /*while({line = read.readLine();line != null && line != "ENDCHAR"}){
          }*/

        }else if({matcher = regexCharCount.matcher(line); matcher.matches()}){
          glyphCount = matcher.group(1).toInt
        }
      }
    }

    /*def getGlyph():Option[Glyph] = TODO
    {

    }*/
  }


  def readBDF(stream:InputStream): BDFInfo =
  {
    val b = new BufferedReader(new InputStreamReader(stream))
    new BDFInfo(b)
  }
}

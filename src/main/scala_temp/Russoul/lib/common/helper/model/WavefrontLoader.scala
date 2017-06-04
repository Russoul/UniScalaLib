package Russoul.lib.common.helper.model

import java.io.{File, FileNotFoundException}
import java.util.Scanner

import Russoul.lib.common.math.linear.{vec2, vec3}

import scala.collection.mutable.ArrayBuffer


class WaveFrontModelLoadingException(msg: String) extends Exception(msg)
{

}

object WavefrontLoader
{
  private final val O_OBJECT = "o"
  private final val V_OBJECT = "v"
  private final val VN_OBJECT = "vn"
  private final val F_OBJECT = "f"
  private final val DOUBLE_SLASH_F_OBJECT = "//"
  private final val SLASH_F_OBJECT = "/"
  private final val VT_OBJECT = "vt"
  private final val MAT_OBJECT = "mtllib"


  @throws[WaveFrontModelLoadingException]
  @throws[NullPointerException]
  @throws[FileNotFoundException]
  def loadModel(path: String): ArrayBuffer[WaveFrontObject] =
  {
    if (path == null) throw new NullPointerException()

    val file = new File(path)

    if (!file.exists()) throw new FileNotFoundException()

    val scanner = new Scanner(file)

    val objects = new ArrayBuffer[WaveFrontObject]

    var curLineNumber = 1

    def atLine() = ", line: " + curLineNumber

    def parseFloat(s: String): Float =
    {
      try {
        s.toFloat
      } catch {
        case e: Exception => throw new WaveFrontModelLoadingException("Float parsing issue, string: " + s + atLine())
      }
    }

    def parseInt(s: String): Int =
    {
      try {
        s.toInt
      } catch {
        case e: Exception => throw new WaveFrontModelLoadingException("Int parsing issue, string: " + s + atLine())
      }
    }

    while (scanner.hasNext) {
      val str = scanner.nextLine()
      if (str.length > 0) {
        var res = ""
        if (str.startsWith(O_OBJECT)) {
          res += str.replace(O_OBJECT + " ", "")
          objects += new WaveFrontObject(res)
        }
        else if (str.startsWith(V_OBJECT) && !str.startsWith(VN_OBJECT) && !str.startsWith(VT_OBJECT)) {
          if (objects.size < 1) throw new WaveFrontModelLoadingException("Attempting to add a vertex without reading an object name first" + atLine())
          res += str.replace(V_OBJECT + " ", "")
          val valuesS = res.split(" ")
          if (valuesS.length < 3) throw new WaveFrontModelLoadingException("Attempting to add a vertex with incorrect data" + atLine())
          val v1 = parseFloat(valuesS(0))
          val v2 = parseFloat(valuesS(1))
          val v3 = parseFloat(valuesS(2))
          objects.last.addVertex(vec3(v1, v2, v3))
        }
        else if (str.startsWith(VN_OBJECT)) {
          if (objects.size < 1) throw new WaveFrontModelLoadingException("Attempting to add a vertex normal without reading an object name first" + atLine())
          res += str.replace(VN_OBJECT + " ", "")
          val valuesS = res.split(" ")
          if (valuesS.length < 3) throw new WaveFrontModelLoadingException("Attempting to add a vertex normal with incorrect data" + atLine())
          val v1 = parseFloat(valuesS(0))
          val v2 = parseFloat(valuesS(1))
          val v3 = parseFloat(valuesS(2))
          objects.last.addNormal(vec3(v1, v2, v3))
        }
        else if (str.startsWith(VT_OBJECT)) {
          if (objects.size < 1) throw new WaveFrontModelLoadingException("Attempting to add a vertex texture without reading an object name first" + atLine())
          res += str.replace(VT_OBJECT + " ", "")
          val valuesS = res.split(" ")
          if (valuesS.length < 2) throw new WaveFrontModelLoadingException("Attempting to add a vertex texture with incorrect data" + atLine())
          val v1 = parseFloat(valuesS(0))
          val v2 = 1 - parseFloat(valuesS(1))
          objects.last.addTextureCoord(vec2(v1, v2))
        }
        else if (str.startsWith(F_OBJECT)) {
          if (objects.size < 1) throw new WaveFrontModelLoadingException("Attempting to add a face without reading an object name first" + atLine())
          res += str.replace(F_OBJECT + " ", "")
          val valuesS = res.split(" ")
          if (valuesS.length < 3) throw new WaveFrontModelLoadingException("Attempting to add a face with incorrect data" + atLine())
          if (res.contains(DOUBLE_SLASH_F_OBJECT)) {
            val fv = new ArrayBuffer[Int]()
            val fn = new ArrayBuffer[Int]()
            for (i <- valuesS) {
              val sp = i.split(DOUBLE_SLASH_F_OBJECT)
              if (sp.length != 2) throw new WaveFrontModelLoadingException("Attempting to add a face with incorrect data" + atLine())
              fv += parseInt(sp(0)) - 1 //OpenGL starts from 0, obj starts from 1
              fn += parseInt(sp(1)) - 1
            }
            val l = objects.last
            if (valuesS.length == 3) {
              l.addVertexNormalIndex(fv(0), fn(0))
              l.addVertexNormalIndex(fv(1), fn(1))
              l.addVertexNormalIndex(fv(2), fn(2))
            }
            else if (valuesS.length == 4) {
              l.addVertexNormalIndex(fv(0), fn(0))
              l.addVertexNormalIndex(fv(1), fn(1))
              l.addVertexNormalIndex(fv(2), fn(2))

              l.addVertexNormalIndex(fv(0), fn(0))
              l.addVertexNormalIndex(fv(2), fn(2))
              l.addVertexNormalIndex(fv(3), fn(3))
            }
            else {
              throw new WaveFrontModelLoadingException("Attempting to add a face with incorrect number of indices" + atLine())
            }
          }
          else if (res.contains(SLASH_F_OBJECT)) {
            //Supports only pos/tex/normal pattern(pos/tex is not supported)
            val fv = new ArrayBuffer[Int]()
            val ft = new ArrayBuffer[Int]()
            val fn = new ArrayBuffer[Int]()
            for (i <- valuesS) {
              val sp = i.split(SLASH_F_OBJECT)
              if (sp.length != 3) throw new WaveFrontModelLoadingException("Attempting to add a face with incorrect data" + atLine())
              fv += parseInt(sp(0)) - 1 //OpenGL starts from 0, obj starts from 1
              ft += parseInt(sp(1)) - 1
              fn += parseInt(sp(2)) - 1
            }
            val l = objects.last
            if (valuesS.length == 3) {
              l.addVertexTextureCoordNormalIndex(fv(0), ft(0), fn(0))
              l.addVertexTextureCoordNormalIndex(fv(1), ft(1), fn(1))
              l.addVertexTextureCoordNormalIndex(fv(2), ft(2), fn(2))
            }
            else if (valuesS.length == 4) {
              l.addVertexTextureCoordNormalIndex(fv(0), ft(0), fn(0))
              l.addVertexTextureCoordNormalIndex(fv(1), ft(1), fn(1))
              l.addVertexTextureCoordNormalIndex(fv(2), ft(2), fn(2))

              l.addVertexTextureCoordNormalIndex(fv(0), ft(0), fn(0))
              l.addVertexTextureCoordNormalIndex(fv(2), ft(2), fn(2))
              l.addVertexTextureCoordNormalIndex(fv(3), ft(3), fn(3))
            }
            else {
              throw new WaveFrontModelLoadingException("Attempting to add a face with incorrect number of indices" + atLine())
            }
          }
        } else if (str.startsWith(MAT_OBJECT)) {
          res += str.replace(MAT_OBJECT + " ", "")
          val mPath = path.replace(file.getName, res)
          val matFile = new File(mPath)
          if (!matFile.exists()) throw new WaveFrontModelLoadingException("Could not load material file" + atLine())
          //TODO i think no practical use would be achieved here :(
        }
      }
      curLineNumber += 1

    }
    scanner.close()

    objects
  }
}

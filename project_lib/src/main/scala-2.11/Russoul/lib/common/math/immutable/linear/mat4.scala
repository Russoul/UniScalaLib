package Russoul.lib.common.math.immutable.linear

import java.nio.FloatBuffer

import Russoul.lib.common.lang.immutable
import Russoul.lib.common.math.TypeClasses.FieldLike
import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._
import Russoul.lib.common.utils.Utilities

/**
  * row vec4 based ! (Directx)
  *
  * immutable
  */
@immutable case class mat4[A](array: Array[A])(implicit ev: FieldLike[A]) {

  private def this() {
    this(new Array[A](16))
  }

  private def this(floats: Seq[A]) {
    this()
    for (i <- floats.indices) {
      array(i) = floats(i)
    }
  }



  def genArray(): Array[A] = {
    val re = new Array[A](16)
    for(i <- 0 until 16) re(i) = array(i)

    re
  }


  @inline def apply(row:Int)(column:Int): A = {
    this.row(row)(column)
  }


  @inline def row(index: Int): Vec4[A] = {
    val s = (index - 1) * 4
    Vec4(array(s), array(s + 1), array(s + 2), array(s + 3))
  }

  def withRow(index: Int, row: Vec4[A]): mat4[A] = {
    val c = copy()

    val s = (index - 1) * 4
    c.array(s) = row(1)
    c.array(s + 1) = row(2)
    c.array(s + 2) = row(3)
    c.array(s + 3) = row(4)
    c
  }

  @inline def column(index: Int): Vec4[A] = {
    val s = index - 1
    Vec4(array(s), array(s + 4), array(s + 8), array(s + 12))
  }

  @inline def get(row: Int, column: Int): A = {
    this.column(column)(row)
  }

  def *(scalar: A): mat4[A] = {
    scalarMultiplication(scalar)
  }

  def *(matrix: mat4[A]): mat4[A] = {
    matrixMultiplication(matrix)
  }

  def scalarMultiplication(scalar: A): mat4[A] = {
    this * scalar
  }

  def matrixMultiplication(matrix: mat4[A]): mat4[A] = {
    this * matrix
  }

  override def toString(): String = {
    var res: String = "mat4\n"


    for(i <- 0 until 4){

      res += "( "
      for(j <- 0 until 4){
        val v = this(i)(j)
        res += v + " "

      }

      res += ")\n"
    }

    res
  }
  

  def transpose(): mat4[A] = {
    val mat = new mat4[A]()

    for(i<- 1 to 4){
      for(j<- 1 to 4){
        mat.array( (j-1)*4 + i-1 ) = array( (i-1)*4 + j-1 )
      }
    }

    mat
  }

  def minor(i: Int, j: Int): Array[A] = {
    val re = new Array[A](9)
    var u = 0
    for (l <- 1 to 4) {
      //row
      for (k <- 1 to 4) {
        //column
        if (i != l && j != k) {
          re(u) = this (l) (k)
          u += 1
        }
      }
    }
    re
  }

  private def minor3x3(i: Int, j: Int, mat: Array[A]): Array[A] = {
    val re = new Array[A](4)
    var u = 0
    for (l <- 1 to 3) {
      //row
      for (k <- 1 to 3) {
        //column
        if (i != l && j != k) {
          re(u) = mat((l - 1) * 3 + (k - 1))
          u += 1
        }
      }
    }
    re
  }

  private def minor2x2(i: Int, j: Int, mat: Array[A]): A = {
    val re = -ev.one
    for (l <- 1 to 2) {
      //row
      for (k <- 1 to 2) {
        //column
        if (i != l && j != k) return mat((l - 1) * 2 + (k - 1))
      }
    }
    re
  }

  private def detmat2x2(mat: Array[A]): A = {
    mat(0) * mat(3) - mat(1) * mat(2)
  }

  private def detmat3x3(mat: Array[A]): A = {
    mat(0) * detmat2x2(minor3x3(1, 1, mat)) - mat(1) * detmat2x2(minor3x3(1, 2, mat)) + mat(2) * detmat2x2(minor3x3(1, 3, mat))
  }

  def determinant(): A = {
    this (1)(1) * detmat3x3(minor(1, 1)) - this (1)(2) * detmat3x3(minor(1, 2)) + this(1)(3) * detmat3x3(minor(1, 3)) - this(1)(4) * detmat3x3(minor(1, 4))
  }

  def cofactor(): mat4[A] = {
    val array = new Array[A](16)

    for (i <- 1 to 4) {
      for (j <- 1 to 4) {
        array( (j-1)*4 + i-1) = (if ((i + j) % 2 == 0) ev.one else -ev.one) * detmat3x3(minor(i, j))
      }
    }
    new mat4(array)
  }

  def inverse(): mat4[A] = {
    val co = this.cofactor()
    val adjoint = co.transpose()
    val det = determinant()
    if (det != 0) {
      adjoint * (ev.one / det)
    }
    else {
      null
    }
  }

  def toSeq(): Seq[A] = {
    Seq[A](array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15))
  }

}

object mat4
{
  def apply[A : FieldLike](floats: A*): mat4[A] =
  {
    new mat4(floats.toSeq)
  }


  def matrixIdentity(): mat4[Float] =
  {

    mat4(1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1)
  }

  def matrixSCALE(x: Float, y: Float, z: Float): mat4[Float] =
  {
    mat4(x, 0, 0, 0,
      0, y, 0, 0,
      0, 0, z, 0,
      0, 0, 0, 1)
  }

  def matrixSCALE(v: Vec3[Float]): mat4[Float] =
  {
    mat4(v.x, 0, 0, 0,
      0, v.y, 0, 0,
      0, 0, v.z, 0,
      0, 0, 0, 1)
  }

  def matrixTRANSLATION(x: Float, y: Float, z: Float): mat4[Float] =
  {
    mat4(1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      x, y, z, 1)
  }

  def matrixTRANSLATION(v: Vec3[Float]): mat4[Float] =
  {
    mat4(1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      v.x, v.y, v.z, 1)
  }

  def matrixROTATION(axis: Vec3[Float], angleInDegrees: Float): mat4[Float] =
  {
    val rad = angleInDegrees * math.Pi / 180
    val cos = math.cos(rad).toFloat
    val sin = math.sin(rad).toFloat
    val x = axis.x
    val y = axis.y
    val z = axis.z
    mat4(cos + x * x * (1 - cos), x * y * (1 - cos) - z * sin, x * z * (1 - cos) + y * sin, 0,
      y * x * (1 - cos) + z * sin, cos + y * y * (1 - cos), y * z * (1 - cos) - x * sin, 0,
      z * x * (1 - cos) - y * sin, z * y * (1 - cos) + x * sin, cos + z * z * (1 - cos), 0,
      0, 0, 0, 1)

  }

  def matrixROTATIONRad[A](axis: Vec3[A], angleInRadians: A)(implicit ev : FieldLike[A]): mat4[A] =
  {
    import Russoul.lib.common.math.TypeClasses.FieldLike.Implicits._

    val cos = ev.cos(angleInRadians)
    val sin = ev.sin(angleInRadians)
    val x = axis.x * axis.x
    val y = axis.y * axis.y
    val z = axis.z * axis.z
    mat4(cos + x * x * (1D.toField - cos), x * y * (1D.toField - cos) - z * sin, x * z * (1D.toField - cos) + y * sin, 0D,
      y * x * (1D.toField - cos) + z * sin, cos + y * y * (1D.toField - cos), y * z * (1D.toField - cos) - x * sin, 0D,
      z * x * (1D.toField - cos) - y * sin, z * y * (1D.toField - cos) + x * sin, cos + z * z * (1D.toField - cos), 0D,
      0D, 0D, 0D, 1D)

  }

  def matrixORTHOGRAPHIC(left: Float, right: Float, bottom: Float, top: Float, near: Float, far: Float): mat4[Float] =
  {

    mat4(2 / (right - left), 0, 0, -(right + left) / (right - left),
      0, 2 / (top - bottom), 0, -(top + bottom) / (top - bottom),
      0, 0, -2 / (far - near), (far + near) / (far - near),
      0, 0, 0, 1).transpose()
  }


  def matrixPERSPECTIVE(angleInDegrees: Float, aspect: Float, near: Float, far: Float): mat4[Float] =
  {
    val top = near * math.tan(math.Pi / 180 * angleInDegrees / 2).toFloat
    val bottom = -top
    val right = top * aspect
    val left = -right

    mat4(2 * near / (right - left), 0, (right + left) / (right - left), 0, //OpenGL form(column-major) not transposed, transposed - row-major form
      0, 2 * near / (top - bottom), (top + bottom) / (top - bottom), 0,
      0, 0, -(far + near) / (far - near), -2 * (far * near) / (far - near),
      0, 0, -1, 0).transpose()
  }

  def matrixPERSPECTIVEDIRECTX(angleInDegrees: Float, aspect: Float, near: Float, far: Float): mat4[Float] = //Directx way doesn't work
  {
    val fov = math.tan(angleInDegrees / 2 / 180 * math.Pi).toFloat
    val r = aspect
    val n = near
    val f = far

    mat4(1 / (r * fov), 0, 0, 0,
      0, 1 / fov, 0, 0,
      0, 0, f / (f - n), 1,
      0, 0, -f * n / (f - n), 0)

  }


  def matrixVIEW(pos: Vec3[Float], target: Vec3[Float], up: Vec3[Float]): mat4[Float] =
  {
    val za = (target - pos).normalize()
    val xa = up.crossProduct(za).normalize()
    val ya = za.crossProduct(xa)
    mat4(
      xa.x, ya.x, za.x, 0,
      xa.y, ya.y, za.y, 0,
      xa.z, ya.z, za.z, 0,
      -xa.dotProduct(pos), -ya.dotProduct(pos), -za.dotProduct(pos), 1)
  }

  def matrixVIEWDir(pos: Vec3[Float], look: Vec3[Float], up: Vec3[Float]): mat4[Float] =
  {
    val za = -look
    val xa = up.crossProduct(za).normalize()
    val ya = za.crossProduct(xa)
    mat4(
      xa.x, ya.x, za.x, 0,
      xa.y, ya.y, za.y, 0,
      xa.z, ya.z, za.z, 0,
      -xa.dotProduct(pos), -ya.dotProduct(pos), -za.dotProduct(pos), 1)
  }

  def matrixVIEW(pos: Vec3[Float], rotX: Float, rotY: Float, rotZ: Float): mat4[Float] =
  {
    var mat = matrixIdentity()
    mat *= matrixTRANSLATION(-pos)
    mat *= matrixROTATION(Vec3(0, 1, 0), rotY)
    mat *= matrixROTATION(Vec3(1, 0, 0), rotX)
    mat *= matrixROTATION(Vec3(0, 0, 1), rotZ)
    mat
  }
}

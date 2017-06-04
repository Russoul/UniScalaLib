package Russoul.lib.common.math.linear

import java.nio.FloatBuffer

import Russoul.lib.common.{Real, immutable}
import Russoul.lib.common.TypeClasses.Field
import Russoul.lib.common.TypeClasses.Field.Implicits._
import Russoul.lib.common.utils.{Arr, Utilities}

/**
  * row vec4 based ! (Directx)
  *
  * immutable
  */
@immutable case class Mat4[@specialized A](private val array: Arr[A])(implicit ev: Field[A]){

  private def this() {
    this(new Arr[A](16))
  }

  private def this(els: Seq[A]) {
    this()
    for (i <- els.indices) {
      array(i) = els(i)
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

  def withRow(index: Int, row: Vec4[A]): Mat4[A] = {
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

  def *(scalar: A): Mat4[A] = {
    scalarMultiplication(scalar)
  }

  def *(matrix: Mat4[A]): Mat4[A] = {
    matrixMultiplication(matrix)
  }

  def scalarMultiplication(scalar: A): Mat4[A] = {
    this * scalar
  }

  def matrixMultiplication(matrix: Mat4[A]): Mat4[A] = {
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
  

  def transpose(): Mat4[A] = {
    val mat = new Mat4[A]()

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

  def cofactor(): Mat4[A] = {
    val array = new Array[A](16)

    for (i <- 1 to 4) {
      for (j <- 1 to 4) {
        array( (j-1)*4 + i-1) = (if ((i + j) % 2 == 0) ev.one else -ev.one) * detmat3x3(minor(i, j))
      }
    }
    new Mat4(array)
  }

  def inverse(): Mat4[A] = {
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

object Mat4
{
  def apply[A : Field](floats: A*): Mat4[A] =
  {
    new Mat4(floats.toSeq)
  }


  //common transformations for V₃ over Real------------------------------------------
  def matrixIdentity(): Mat4[Real] =
  {

    Mat4(1D, 0D, 0D, 0D,
      0D, 1D, 0D, 0D,
      0D, 0D, 1D, 0D,
      0D, 0D, 0D, 1D)
  }

  def matrixSCALE(x: Real, y: Real, z: Real): Mat4[Real] =
  {
    Mat4[Real](x, 0D, 0D, 0D,
      0D, y, 0D, 0D,
      0D, 0D, z, 0D,
      0D, 0D, 0D, 1D)
  }

  def matrixSCALE(v: Vec3[Real]): Mat4[Real] =
  {
    Mat4[Real](v.x, 0, 0, 0,
      0, v.y, 0, 0,
      0, 0, v.z, 0,
      0, 0, 0, 1)
  }

  def matrixTRANSLATION(x: Real, y: Real, z: Real): Mat4[Real] =
  {
    Mat4[Real](1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      x, y, z, 1)
  }

  def matrixTRANSLATION(v: Vec3[Real]): Mat4[Real] =
  {
    Mat4[Real](1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      v.x, v.y, v.z, 1)
  }

  def matrixROTATION(axis: Vec3[Real], angleInDegrees: Real): Mat4[Real] =
  {
    val rad = angleInDegrees * math.Pi / 180
    val cos = math.cos(rad)
    val sin = math.sin(rad)
    val x = axis.x
    val y = axis.y
    val z = axis.z
    Mat4[Real](cos + x * x * (1 - cos), x * y * (1 - cos) - z * sin, x * z * (1 - cos) + y * sin, 0,
      y * x * (1 - cos) + z * sin, cos + y * y * (1 - cos), y * z * (1 - cos) - x * sin, 0,
      z * x * (1 - cos) - y * sin, z * y * (1 - cos) + x * sin, cos + z * z * (1 - cos), 0,
      0, 0, 0, 1)

  }

  def matrixROTATIONRad(axis: Vec3[Real], angleInRadians: Real): Mat4[Real] =
  {

    val cos = Math.cos(angleInRadians)
    val sin = Math.sin(angleInRadians)
    val x = axis.x * axis.x
    val y = axis.y * axis.y
    val z = axis.z * axis.z
    Mat4[Real](cos + x * x * (1D - cos), x * y * (1D - cos) - z * sin, x * z * (1D - cos) + y * sin, 0D,
      y * x * (1D - cos) + z * sin, cos + y * y * (1D - cos), y * z * (1D - cos) - x * sin, 0D,
      z * x * (1D - cos) - y * sin, z * y * (1D - cos) + x * sin, cos + z * z * (1D - cos), 0D,
      0D, 0D, 0D, 1D)

  }

  def matrixORTHOGRAPHIC(left: Real, right: Real, bottom: Real, top: Real, near: Real, far: Real): Mat4[Real] =
  {

    Mat4[Real](2 / (right - left), 0, 0, -(right + left) / (right - left),
      0, 2 / (top - bottom), 0, -(top + bottom) / (top - bottom),
      0, 0, -2 / (far - near), (far + near) / (far - near),
      0, 0, 0, 1).transpose()
  }


  def matrixPERSPECTIVE(angleInDegrees: Real, aspect: Real, near: Real, far: Real): Mat4[Real] =
  {
    val top:Real = near * math.tan(math.Pi / 180 * angleInDegrees / 2)
    val bottom = -top
    val right = top * aspect
    val left = -right

    Mat4(2 * near / (right - left), 0, (right + left) / (right - left), 0, //OpenGL form(column-major) not transposed, transposed - row-major form
      0, 2 * near / (top - bottom), (top + bottom) / (top - bottom), 0,
      0, 0, -(far + near) / (far - near), -2 * (far * near) / (far - near),
      0, 0, -1, 0).transpose()
  }

  def matrixPERSPECTIVEDIRECTX(angleInDegrees: Real, aspect: Real, near: Real, far: Real): Mat4[Real] = //Directx way doesn't work
  {
    val fov = math.tan(angleInDegrees / 2 / 180 * math.Pi)
    val r = aspect
    val n = near
    val f = far

    Mat4(1 / (r * fov), 0, 0, 0,
      0, 1 / fov, 0, 0,
      0, 0, f / (f - n), 1,
      0, 0, -f * n / (f - n), 0)

  }


  def matrixVIEW(pos: Vec3[Real], target: Vec3[Real], up: Vec3[Real]): Mat4[Real] =
  {
    val za = (target - pos).normalize()
    val xa = up.crossProduct(za).normalize()
    val ya = za.crossProduct(xa)
    Mat4[Real](
      xa.x, ya.x, za.x, 0D,
      xa.y, ya.y, za.y, 0D,
      xa.z, ya.z, za.z, 0D,
      -xa.dotProduct(pos), -ya.dotProduct(pos), -za.dotProduct(pos), 1)
  }

  def matrixVIEWDir(pos: Vec3[Real], look: Vec3[Real], up: Vec3[Real]): Mat4[Real] =
  {
    val za = -look
    val xa = up.crossProduct(za).normalize()
    val ya = za.crossProduct(xa)
    Mat4[Real](
      xa.x, ya.x, za.x, 0D,
      xa.y, ya.y, za.y, 0D,
      xa.z, ya.z, za.z, 0D,
      -xa.dotProduct(pos), -ya.dotProduct(pos), -za.dotProduct(pos), 1)
  }

  def matrixVIEW(pos: Vec3[Real], rotX: Real, rotY: Real, rotZ: Real): Mat4[Real] =
  {
    var mat = matrixIdentity()
    mat *= matrixTRANSLATION(-pos)
    mat *= matrixROTATION(Vec3(0D, 1D, 0D), rotY)
    mat *= matrixROTATION(Vec3(1D, 0D, 0D), rotX)
    mat *= matrixROTATION(Vec3(0D, 0D, 1D), rotZ)
    mat
  }
}

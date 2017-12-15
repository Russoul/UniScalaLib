package russoul.lib.common.voxel

import russoul.lib.common._
import spire.algebra._
import spire.math._
import spire.implicits._

import scala.reflect.ClassTag

/**
  * Created by russoul on 19.07.2017.
  */

/**
  *
  * @param a world size of a side of a grid square
  * @param sizeX number of squares along X axis
  * @param sizeY number of squares along Y axis
  * @tparam A
  */
class VoxelGrid2[@sp(Float, Double) A : ClassTag] (val a: A, val sizeX: Int, val sizeY: Int){

  def verticesX = sizeX + 1
  def verticesY = sizeY + 1

  val grid = new Array[A](verticesX * verticesY)
  val normals = new Array[Vec2[A]](verticesX * verticesY)
  val vertices = new Array[Vec2[A]](sizeX * sizeY)


  def get(x: Int, y: Int) : A = {
    grid(y * verticesX + x)
  }

  def set(x: Int, y: Int, value: A) : Unit = {
    grid(y * verticesX + x) = value
  }

  /**
    *
    * @return local to voxel grid coordinates of the point
    */
  def getPoint(x: Int, y: Int)(implicit field : Field[A]) : Vec2[A] = {
    Vec2[A](a * field.fromInt(x), a * field.fromInt(y))
  }

}

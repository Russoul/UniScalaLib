package Russoul.lib.context.scene

import java.lang

import Russoul.lib.common.math.CollisionEngine
import Russoul.lib.common.math.geometry.simple.{AABB, Ray}
import Russoul.lib.common.math.linear.vec3
import Russoul.lib.common.utils.vector
import Russoul.lib.context.scene.structure.{ChunkGrid, World}

/**
  * Created by wzlom on 23.09.2016.
  *
  * Axis aligned cube !
  */
class VoxelGrid(val center:vec3, val extent:Float, val voxelSize:Float,val world:World)
{
  def getVoxelSize() = voxelSize

  def densitiesOnSide() = (2*extent / voxelSize).toInt

  def voxelsOnSide() = densitiesOnSide() + 1  //2 voxels on each density line intersect the grid bounding

  //TODO grid is a cube => checks may be simplified for AABC
  //TODO accelerate even more !!, this is important !
  /**
    *
    * @param ray
    * @param subTimes
    * @param re
    * @param curSub
    * @param center
    * @param extent
    */
  def rayTrace(ray:Ray, subTimes:Int, re : vector[(Int,Int,Int)], curSub:Int = 1, center:vec3 = this.center, extent:Float = this.extent): Unit =
  {


    val bounds = genOctreeNodeBounds(center, extent)
    for(bound <- bounds){
      val check = CollisionEngine.checkRayBox(ray, bound).isDefined

      if(check){

        if(bound.extent.x*2 <= ChunkGrid.GENERATEDVOXELSIZE){
          val local = bound.center - this.min()

          val xx = (local.x/ChunkGrid.GENERATEDVOXELSIZE).toInt
          val yy = (local.y/ChunkGrid.GENERATEDVOXELSIZE).toInt
          val zz = (local.z/ChunkGrid.GENERATEDVOXELSIZE).toInt

          val size = if((bound.extent.x / ChunkGrid.GENERATEDVOXELSIZE) == lang.Math.ceil(bound.extent.x / ChunkGrid.GENERATEDVOXELSIZE)) (bound.extent.x / ChunkGrid.GENERATEDVOXELSIZE).toInt else  (bound.extent.x / ChunkGrid.GENERATEDVOXELSIZE).toInt + 1

          for(x <- xx-size to xx+size if x >= 0) {
            for(y <- yy-size to yy+size if y >= 0) {
              for(z <- zz-size to zz+size if z >= 0) {
                val voxel = voxelAt(x,y,z)
                val check = CollisionEngine.checkRayBox(ray, new AABB(voxel, vec3(ChunkGrid.GENERATEDVOXELSIZE/2,ChunkGrid.GENERATEDVOXELSIZE/2,ChunkGrid.GENERATEDVOXELSIZE/2))).isDefined
                if(check) re += (x,y,z)
              }
            }
          }
        }else{
          if(curSub < subTimes){
            rayTrace(ray, subTimes, re, curSub + 1, bound.center, bound.extent.x)
          }else{
            val local = bound.center - this.min()

            val xx = (local.x/ChunkGrid.GENERATEDVOXELSIZE).toInt
            val yy = (local.y/ChunkGrid.GENERATEDVOXELSIZE).toInt
            val zz = (local.z/ChunkGrid.GENERATEDVOXELSIZE).toInt

            val size = if((bound.extent.x / ChunkGrid.GENERATEDVOXELSIZE) == lang.Math.ceil(bound.extent.x / ChunkGrid.GENERATEDVOXELSIZE)) (bound.extent.x / ChunkGrid.GENERATEDVOXELSIZE).toInt else  (bound.extent.x / ChunkGrid.GENERATEDVOXELSIZE).toInt + 1


            for(x <- xx-size to xx+size if x >= 0) {
              for(y <- yy-size to yy+size if y >= 0) {
                for(z <- zz-size to zz+size if z >= 0) {
                  val voxel = voxelAt(x,y,z)
                  val check = CollisionEngine.checkRayBox(ray, new AABB(voxel, vec3(ChunkGrid.GENERATEDVOXELSIZE/2,ChunkGrid.GENERATEDVOXELSIZE/2,ChunkGrid.GENERATEDVOXELSIZE/2))).isDefined
                  if(check) re += (x,y,z)
                }
              }
            }
          }
        }
      }
    }

  }


  def bruteRaytrace(ray:Ray): vector[(Int,Int,Int)] =
  {
    val res = vector[(Int,Int,Int)]()

    val extent = vec3(ChunkGrid.GENERATEDVOXELSIZE/2,ChunkGrid.GENERATEDVOXELSIZE/2,ChunkGrid.GENERATEDVOXELSIZE/2)

    for(i <- 0 until voxelsOnSide()){
      for(j <- 0 until voxelsOnSide()){
        for(k <- 0 until voxelsOnSide()){
          val v = voxelAt(i,j,k)

          val inter = CollisionEngine.checkRayBox(ray, new AABB(v, extent))

          if(inter.isDefined) res += (i,j,k)
        }
      }
    }

    res
  }

  def genOctreeNodeBounds(center:vec3 = this.center, extent:Float = this.extent):vector[AABB]  =
  {
    val re = vector.ofSize[AABB](8)


    val mini = center - vec3(extent,extent,extent)

    val ext = extent/2
    val e = vec3(ext,ext,ext)

    re += new AABB(mini + vec3(ext,ext,ext),e)
    re += new AABB(mini + vec3(ext,ext,3*ext),e)
    re += new AABB(mini + vec3(ext,3*ext,ext),e)
    re += new AABB(mini + vec3(ext,3*ext,3*ext),e)
    re += new AABB(mini + vec3(3*ext,ext,ext),e)
    re += new AABB(mini + vec3(3*ext,ext,3*ext),e)
    re += new AABB(mini + vec3(3*ext,3*ext,ext),e)
    re += new AABB(mini + vec3(3*ext,3*ext,3*ext),e)

    re

  }

  def densityCount() =
  {
    val d = densitiesOnSide()
    d*d*d
  }

  def voxelCount() =
  {
    val d = voxelsOnSide()
    d*d*d
  }

  val densities = new Array[Float](densityCount()) //linear array, XYZ
  val colors = new Array[Int](densityCount()) //linear array, XYZ

  //default def
  {
    for(x <- 0 until densitiesOnSide){
      for(y <- 0 until densitiesOnSide){
        for(z <- 0 until densitiesOnSide){
          setColor(x,y,z, 0x592d17)
          setDensity(x,y,z,1)
        }
      }
    }
  }


  def gen(func: (vec3)=>Float ): Unit =
  {


    for(x <- 0 until densitiesOnSide){
      for(y <- 0 until densitiesOnSide){
        for(z <- 0 until densitiesOnSide){
          //global bos
          val pos  = densityAt(x,y,z)
          setDensity(x,y,z, func(pos))
        }
      }
    }
  }

  /**
    *
    * @return densities count starting from bound min
    */
  @inline def density(r:Int, u:Int, l:Int):Float =
  {
    val d = densitiesOnSide()
    densities(l + u*d + r*d*d)
  }

  @inline def density( v:(Int,Int,Int) ):Float = density(v._1, v._2, v._3)

  def color(r:Int, u:Int, l:Int):Int =
  {
    val d = densitiesOnSide()
    colors(l + u*d + r*d*d)
  }

  def min() = center - vec3(extent,extent,extent)
  def max() = center + vec3(extent,extent,extent)

  def densityAt(r:Int, u:Int, l:Int):vec3 =
  {
    min() + vec3(r*voxelSize+voxelSize/2, u*voxelSize+voxelSize/2, l*voxelSize+voxelSize/2)
  }

  def voxelAt(r:Int, u:Int, l:Int):vec3 =
  {
    min() + vec3(r*voxelSize, u*voxelSize, l*voxelSize)
  }


  /**
    *
    * @param p not checked for belonging to grid bounding box
    * @return
    */
  def positionFloored(p:vec3):(Int,Int,Int) =
  {
    val iii = (p - min())/voxelSize

    (iii.x.toInt, iii.y.toInt, iii.z.toInt)
  }


  def setDensity(iii:(Int,Int,Int), den:Float):Unit =
  {
    val d = densitiesOnSide()
    densities(iii._3 + iii._2*d + iii._1*d*d) = den
  }

  def setDensity(r:Int, u:Int, l:Int, den:Float):Unit =
  {
    val d = densitiesOnSide()
    densities(l + u*d + r*d*d) = den
  }

  def setColor(r:Int, u:Int, l:Int, color:Int):Unit =
  {
    val d = densitiesOnSide()
    colors(l + u*d + r*d*d) = color
  }

  def isVoxelEmpty(triple:(Int, Int, Int) ): Boolean =
  {
    isVoxelEmpty(triple._1, triple._2, triple._3)
  }

  def isVoxelEmpty(r:Int, u:Int, l:Int): Boolean =
  {
    var empties = 0

    val intersections = getChunksVoxelIntersects(r,u,l)
    for(pair <- intersections){ //always 8
      if(world.getChunk(pair._1) == null) empties += 1
      else{
        if(world.getChunk(pair._1).grid.density(pair._2) > 0) empties += 1
      }
    }

    empties == 8


  }

  def isVoxelSolid(tr:(Int,Int,Int)): Boolean = isVoxelSolid(tr._1, tr._2, tr._3)

  def isVoxelSolid(r:Int, u:Int, l:Int): Boolean ={
    val intersections = getChunksVoxelIntersects(r,u,l)
    for(pair <- intersections){ //always 8
      if(world.getChunk(pair._1) != null)
        if(world.getChunk(pair._1).grid.density(pair._2) <= 0) return true

    }

    false

  }

  /**
    *
    * @param r
    * @param u
    * @param l
    * @return chunks of intersection(including THIS chunk) and their respected positions of densities of intersection
    *         without chunk existence check !!!
    *
    *         outputs in the same ways as Marching cubes inputs
    */
  //TODO TEST !!!
  def getChunksVoxelIntersects(r:Int, u:Int, l:Int): vector[(vec3,(Int,Int,Int))] =
  {
    val maxVoxel = voxelsOnSide() - 1
    val maxDensity = densitiesOnSide() - 1
    val cs = CVoxel.CHUNK_SIZE_IN_UNITS

    val here = center
    val right = vec3(cs,0,0)
    val up = vec3(0,cs,0)
    val look = vec3(0,0,cs)

    val re = vector.ofSize[(vec3,(Int,Int,Int))](8)

    //27 variants :(
    if(r == maxVoxel) //8 intersections
    {
      if(u == maxVoxel){
        if(l == maxVoxel){
          

          val tor = center + right
          val torl = center + right + look
          val tol = center + look

          val toru = center + right + up
          val torlu = center + right + look + up
          val tolu = center + look + up
          val tou = center + up

          re += (here, (r-1,u-1,l-1))//self
          re += (tor, (0,maxDensity,maxDensity))
          re += (torl, (0,maxDensity,0))
          re += (tol, (maxDensity, maxDensity, 0))
          re += (tou, (maxDensity,0,maxDensity))
          re += (toru, (0,0,maxDensity))
          re += (torlu, (0,0,0))
          re += (tolu, (maxDensity, 0, 0))

        }else if(l == 0)
        {


          val tor = center + right
          val torl = center + right - look
          val tol = center - look

          val toru = center + right + up
          val torlu = center + right - look + up
          val tolu = center - look + up
          val tou = center + up

          re += (tol, (maxDensity, maxDensity, maxDensity))
          re += (torl, (0,maxDensity,maxDensity))
          re += (tor, (0,maxDensity,0))
          re += (here, (r-1,u-1,0))//self
          re += (tolu, (maxDensity, 0, maxDensity))
          re += (torlu, (0,0,maxDensity))
          re += (toru, (0,0,0))
          re += (tou, (maxDensity,0,0))

        }else{ //4 vars

          val tor = center + right
          val toru = center + right + up
          val tou = center + up

          re += (here, (maxDensity,maxDensity,l-1))//self
          re += (tor, (0,maxDensity,l-1))
          re += (tor, (0,maxDensity,l))
          re += (here, (maxDensity,maxDensity,l))//self
          re += (tou, (maxDensity,0,l-1))
          re += (toru, (0,0,l-1))
          re += (toru, (0,0,l))
          re += (tou, (maxDensity,0,l))

        }

      }else if(u == 0){
        if(l == maxVoxel){ //8 vars


          re += (center - up, (maxDensity,maxDensity,maxDensity))
          re += (center + right - up, (0,maxDensity,maxDensity))
          re += (center + right + look - up, (0,maxDensity,0))
          re += (center + look - up, (maxDensity,maxDensity,0))
          re += (here, (maxDensity, 0, maxDensity))
          re += (center + right, (0,0,maxDensity))
          re += (center + right + look, (0,0,0))
          re += (center + look, (maxDensity,0,0))


        }else if(l == 0){ //8 vars

          re += (center - look - up, (maxDensity,maxDensity,maxDensity))
          re += (center + right - look - up, (0,maxDensity,maxDensity))
          re += (center + right - up, (0,maxDensity,0))
          re += (center - up, (maxDensity,maxDensity,0))
          re += (center - look, (maxDensity,0,maxDensity))
          re += (center + right - look, (0,0,maxDensity))
          re += (center + right, (0,0,0))
          re += (here, (maxDensity, 0, 0))


        }else{ //4 vars

          val tor = center + right
          val tord = center + right - up
          val tod = center - up


          re += (tod, (maxDensity, maxDensity, l-1))
          re += (tord, (0, maxDensity, l-1))
          re += (tord, (0, maxDensity, l))
          re += (tod, (maxDensity, maxDensity, l))
          re += (here, (maxDensity, 0, l-1))
          re += (tor, (0, 0, l-1))
          re += (tor, (0, 0, l))
          re += (here, (maxDensity, 0, l))


        }
      }else{ // 0 < u < maxVoxel
        if(l == maxVoxel){ //8 vars

          val tor = center + right
          val torl = center + right + look
          val tol = center + look

          re += (here, (maxDensity, u-1, maxDensity))
          re += (tor, (0, u-1, maxDensity))
          re += (torl, (0, u-1, 0))
          re += (tol, (maxDensity, u-1, 0))
          re += (here, (maxDensity, u, maxDensity))
          re += (tor, (0, u, maxDensity))
          re += (torl, (0, u, 0))
          re += (tol, (maxDensity, u, 0))


        }else if(l == 0){ //8 vars


          val tor = center + right
          val torl = center + right - look
          val tol = center - look

          re += (tol, (maxDensity, u-1, maxDensity))
          re += (torl, (0, u-1,maxDensity))
          re += (tor, (0, u-1, 0))
          re += (here, (maxDensity, u-1, 0))
          re += (tol, (maxDensity, u, maxDensity))
          re += (torl, (0, u, maxDensity))
          re += (tor, (0, u, 0))
          re += (here, (maxDensity, u, 0))


        }else{ //4 vars
          val tor = center + right

          re += (here, (maxDensity, u-1, l-1))
          re += (tor, (0, u-1, l-1))
          re += (tor, (0, u-1, l))
          re += (here, (maxDensity, u-1, l))
          re += (here, (maxDensity, u, l-1))
          re += (tor, (0, u, l-1))
          re += (tor, (0, u, l))
          re += (here, (maxDensity, u, l))


        }
      }


    }else if(r == 0){
      if(u == maxVoxel){
        if(l == maxVoxel){

          val tor = center - right
          val torl = center - right + look
          val tol = center + look

          val toru = center - right + up
          val torlu = center - right + look + up
          val tolu = center + look + up



          re += (tor, (maxDensity, maxDensity, maxDensity))
          re += (here, (0, maxDensity, maxDensity))
          re += (tol, (0, maxDensity, 0))
          re += (torl, (maxDensity, maxDensity, 0))
          re += (toru, (maxDensity, 0, maxDensity))
          re += (center + up, (0,0,maxDensity))
          re += (tolu, (0, 0, 0))
          re += (torlu, (maxDensity, 0, 0))



        }else if(l == 0){

          val tor = center - right
          val torl = center - right - look
          val tol = center - look

          val toru = center - right + up
          val torlu = center - right - look + up
          val tolu = center - look + up


          re += (torl, (maxDensity, maxDensity, maxDensity))
          re += (tol, (0, maxDensity, maxDensity))
          re += (here, (0, maxDensity, 0))
          re += (tor, (maxDensity, maxDensity, 0))
          re += (torlu, (maxDensity, 0, maxDensity))
          re += (tolu, (0, 0, maxDensity))
          re += (center + up, (0,0,0))
          re += (toru, (maxDensity, 0, 0))


        }else{


          val tor = center - right
          val toru = center - right + up
          val tou = center + up

          re += (tor, (maxDensity, maxDensity, l-1))
          re += (here, (0, maxDensity, l-1))
          re += (here, (0, maxDensity, l))
          re += (tor, (maxDensity, maxDensity, l))
          re += (toru, (maxDensity, 0, l-1))
          re += (tou, (0, 0, l-1))
          re += (tou, (0, 0, l))
          re += (toru, (maxDensity, 0, l))


        }
      }else if(u == 0){
        if(l == maxVoxel){

          val tor = center - right
          val torl = center - right + look
          val tol = center + look

          val toru = center - right - up
          val torlu = center - right + look - up
          val tolu = center + look - up

          re += (toru, (maxDensity, maxDensity, maxDensity))
          re += (center - up, (0,maxDensity,maxDensity))
          re += (tolu, (0, maxDensity, 0))
          re += (torlu, (maxDensity, maxDensity, 0))
          re += (tor, (maxDensity, 0, maxDensity))
          re += (here, (0, 0, maxDensity))
          re += (tol, (0, 0, 0))
          re += (torl, (maxDensity, 0, 0))

        }else if(l == 0){

          val tor = center - right
          val torl = center - right - look
          val tol = center - look

          val toru = center - right - up
          val torlu = center - right - look - up
          val tolu = center - look - up

          re += (torlu, (maxDensity, maxDensity, maxDensity))
          re += (tolu, (0, maxDensity, maxDensity))
          re += (center - up, (0,maxDensity,0))
          re += (toru, (maxDensity, maxDensity, 0))
          re += (torl, (maxDensity, 0, maxDensity))
          re += (tol, (0, 0, maxDensity))
          re += (here, (0, 0, 0))
          re += (tor, (maxDensity, 0, 0))

        }else{


          val tor = center - right
          val tord = center - right - up
          val tod = center - up


          re += (tord, (maxDensity, maxDensity, l-1))
          re += (tod, (0, maxDensity, l-1))
          re += (tod, (0, maxDensity, l))
          re += (tord, (maxDensity, maxDensity, l))
          re += (tor, (maxDensity, 0, l-1))
          re += (here, (0, 0, l-1))
          re += (here, (0, 0, l))
          re += (tor, (maxDensity, 0, l))
        }
      }else{
        if(l == maxVoxel){ //8 vars


          val tor = center - right
          val torl = center - right + look
          val tol = center + look


          re += (tor, (maxDensity, u-1, maxDensity))
          re += (here, (0, u-1, maxDensity))
          re += (tol, (0, u-1, 0))
          re += (torl, (maxDensity, u-1, 0))
          re += (tor, (maxDensity, u, maxDensity))
          re += (here, (0, u, maxDensity))
          re += (tol, (0, u, 0))
          re += (torl, (maxDensity, u, 0))

        }else if(l == 0){ //8 vars


          val tor = center - right
          val torl = center - right - look
          val tol = center - look

          re += (torl, (maxDensity, u-1,maxDensity))
          re += (tol, (0, u-1, maxDensity))
          re += (here, (0, u-1, 0))
          re += (tor, (maxDensity, u-1, 0))
          re += (torl, (maxDensity, u, maxDensity))
          re += (tol, (0, u, maxDensity))
          re += (here, (0, u, 0))
          re += (tor, (maxDensity, u, 0))

        }else{ //4 vars

          val tor = center - right


          re += (tor, (maxDensity, u-1, l-1))
          re += (here, (0, u-1, l-1))
          re += (here, (0, u-1, l))
          re += (tor, (maxDensity, u-1, l))

          re += (tor, (maxDensity, u, l-1))
          re += (here, (0, u, l-1))
          re += (here, (0, u, l))
          re += (tor, (maxDensity, u, l))

        }
      }
    }else{ // 0 < t < maxVoxel
      if(u == maxVoxel){
        if(l == maxVoxel){

          val tol = center + look
          val tolu = center + look + up
          val tou = center + up

          re += (here, (r-1, maxDensity, maxDensity))
          re += (here, (r, maxDensity, maxDensity))
          re += (tol, (r, maxDensity, 0))
          re += (tol, (r-1, maxDensity, 0))
          re += (tou, (r-1, 0, maxDensity))
          re += (tou, (r, 0, maxDensity))
          re += (tolu, (r, 0, 0))
          re += (tolu, (r-1, 0, 0))


        }else if(l == 0){
          val tol = center - look
          val tolu = center - look + up
          val tou = center + up

          re += (tol, (r-1, maxDensity, maxDensity))
          re += (tol, (r, maxDensity, maxDensity))
          re += (here, (r, maxDensity, 0))
          re += (here, (r-1, maxDensity, 0))
          re += (tolu, (r-1, 0, maxDensity))
          re += (tolu, (r, 0, maxDensity))
          re += (tou, (r, 0, 0))
          re += (tou, (r-1, 0, 0))


        }else{

          val tou = center + up

          re += (here, (r-1, maxDensity, l-1))
          re += (here, (r, maxDensity, l-1))
          re += (here, (r, maxDensity, l))
          re += (here, (r-1, maxDensity, l))
          re += (tou, (r-1, 0, l-1))
          re += (tou, (r, 0, l-1))
          re += (tou, (r, 0, l))
          re += (tou, (r-1, 0, l))

        }
      }else if(u == 0){
        if(l == maxVoxel){


          val tol = center + look
          val tolu = center + look - up
          val tou = center - up

          re += (tou, (r-1, maxDensity, maxDensity))
          re += (tou, (r, maxDensity, maxDensity))
          re += (tolu, (r, maxDensity, 0))
          re += (tolu, (r-1, maxDensity, 0))
          re += (here, (r-1, 0, maxDensity))
          re += (here, (r, 0, maxDensity))
          re += (tol, (r, 0, 0))
          re += (tol, (r-1, 0, 0))


        }else if(l == 0){


          val tol = center - look
          val tolu = center - look - up
          val tou = center - up


          re += (tolu, (r-1, maxDensity, maxDensity))
          re += (tolu, (r, maxDensity, maxDensity))
          re += (tou, (r, maxDensity, 0))
          re += (tou, (r-1, maxDensity, 0))
          re += (tol, (r-1, 0, maxDensity))
          re += (tol, (r, 0, maxDensity))
          re += (here, (r, 0, 0))
          re += (here, (r-1, 0, 0))

        }else{

          val tou = center - up

          re += (tou, (r-1, maxDensity, l-1))
          re += (tou, (r, maxDensity, l-1))
          re += (tou, (r, maxDensity, l))
          re += (tou, (r-1, maxDensity, l))
          re += (here, (r-1, 0, l-1))
          re += (here, (r, 0, l-1))
          re += (here, (r, 0, l))
          re += (here, (r-1, 0, l))

        }
      }else{
        if(l == maxVoxel){


          val tol = center + look

          re += (here, (r-1, u-1, maxDensity))
          re += (here, (r, u-1, maxDensity))
          re += (tol, (r, u-1, 0))
          re += (tol, (r-1, u-1, 0))
          re += (here, (r-1, u, maxDensity))
          re += (here, (r, u, maxDensity))
          re += (tol, (r, u, 0))
          re += (tol, (r-1, u, 0))

        }else if(l == 0){


          val tol = center - look


          re += (tol, (r-1, u-1, maxDensity))
          re += (tol, (r, u-1, maxDensity))
          re += (here, (r, u-1, 0))
          re += (here, (r-1, u-1, 0))
          re += (tol, (r-1, u, maxDensity))
          re += (tol, (r, u, maxDensity))
          re += (here, (r, u, 0))
          re += (here, (r-1, u, 0))

        }else{
          re += (here, (r-1, u-1, l-1))
          re += (here, (r, u-1, l-1))
          re += (here, (r, u-1, l))
          re += (here, (r-1, u-1, l))
          re += (here, (r-1, u, l-1))
          re += (here, (r, u, l-1))
          re += (here, (r, u, l))
          re += (here, (r-1, u, l))
        }
      }
    }


    re
  }


}

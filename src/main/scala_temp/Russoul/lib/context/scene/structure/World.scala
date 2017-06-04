package Russoul.lib.context.scene.structure

import Russoul.engine.core.Engine
import Russoul.lib.common.math.Math._
import Russoul.lib.common.math.{CollisionEngine, Math}
import Russoul.lib.common.math.geometry.complex.RegularDodecahedron
import Russoul.lib.common.math.geometry.simple._
import Russoul.lib.common.math.linear.vec3
import Russoul.lib.common.utils.{Timer, map, vector}
import Russoul.lib.context.scene.{CVoxel, PointLight, VOOctree}
import Russoul.lib.context.utils.SimplexNoise
import libnoiseforjava.module.{Perlin, Voronoi}

import scala.language.postfixOps
import scala.util.Random
import scala.util.control.{Breaks, ControlThrowable}

/**
  * Created by Russoul on 19.07.2016.
  */




class World
{
  private val chunks= map.empty[vec3, ChunkGrid]


  //LIGHTING
  private val pointLights = new vector[PointLight]() //TODO currently global per world
  def lightCount = pointLights.length

  {//constructor
    val light0 = new PointLight(vec3(4, 4, 4), vec3(50, 50, 0))
    val light1 = new PointLight(vec3(4, 4, -4), vec3(50, 50, 0)) //yellow lights
    val light2 = new PointLight(vec3(0, 5, 0), vec3(50, 50, 0))
    val light3 = new PointLight(vec3(32, 7, 0), vec3(50, 50, 0))

    pointLights += light0
    pointLights += light1
    pointLights += light2
    pointLights += light3
  }
  //...........................


  def getLight(n:Int) = pointLights(n)
  def addChunk(pos:vec3, chunk:ChunkGrid): ChunkGrid ={ chunks(pos) = chunk; chunk}
  def getChunk(pos:vec3):ChunkGrid =
  {
    val op = chunks.get(pos)

    if(op.isDefined) op.get else null
  }


  def getChunkPosition(camPos:vec3): vec3 =
  {
    Math.roundTo2Power(camPos, CVoxel.CHUNK_SIZE_IN_UNITS_POWER)
  }

  def hasChunk(pos:vec3):Boolean =
  {
    chunks.get(pos).isDefined
  }

  def clearChunks() = chunks.clear()

  def getAllChunks() = chunks.values

  object Generator
  {

    val noise = new SimplexNoise(Timer.getTimeNano().toInt)

    def genDensitySimple(v:vec3) = ((v.x*v.x) + (v.y-4)*(v.y-4) + v.z*v.z - 9) inv
    def genDensity(x:Float, y:Float, z:Float):Float = noise.noise(x,y,z).toFloat
    def genDensity(pos:vec3) = noise.noise(pos.x, pos.y, pos.z).toFloat
    def gen1D(x:Float) = noise.noise(x,0)
  }





  /*/**
    *TODO SLOW !!!
    * @param ray
    * @param visualExtent
    * @param func
    * @return
    *
    * processes chunks uniquely
    */
  def processRayTraceChunks(ray:Ray, visualExtent:Int, @throws(classOf[ControlThrowable]) func:(ChunkGrid,Float,Rectangle)=>Any): Boolean =
  {
    val processed = vector[ChunkGrid]()

    processRayTraceChunks(ray, visualExtent, (block)=>
    {
      val tree = block.getTree()
      val break = tree.processAllIntersectionsRecursive(ray, func, processed)

      if(!break) Breaks.break()
    })
  }


  def processRayTraceChunks(ray:Ray, visualExtent:Int, @throws(classOf[ControlThrowable]) func:(ChunkGrid)=>Any):Boolean =
  {
    Breaks.tryBreakable
    {
      val c = Math.roundTo2Power(ray.start, CVoxel.CHUNK_SIZE_IN_UNITS_POWER)
      val x = c.x.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
      val y = c.y.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
      val z = c.z.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt


      for(i:Int <- x-visualExtent to x+visualExtent) for(j:Int  <- y-visualExtent to y+visualExtent) for(k:Int  <- z-visualExtent to z+visualExtent){
        val blockPos = vec3(i*CVoxel.CHUNK_SIZE_IN_UNITS,j*CVoxel.CHUNK_SIZE_IN_UNITS,k*CVoxel.CHUNK_SIZE_IN_UNITS)
        val block = getChunk(blockPos)

        if(block != null){
          val bound = block.getTree().genBound()
          if(CollisionEngine.checkRayBox(ray, bound).isDefined){
            func(block)
          }
        }

      }

      true
    }catchBreak
    {
      false
    }

  }*/

  /*def generateRegularDodecahedronPlanet(regularDodecahedron:RegularDodecahedron): Boolean =
  {
    val vertices = regularDodecahedron.genVertices()
    val pentagonals = regularDodecahedron.genPentagonals(vertices)
    val lLook = regularDodecahedron.genLength1(vertices)
    val lRight = regularDodecahedron.genLength2(vertices)

    val lUp = 1F

    Breaks.tryBreakable{
      for(i <- pentagonals.indices) //each surface
      {
        val pentagonal = pentagonals(i)

        val right = (pentagonal._7 - pentagonal._4).normalize()
        val up = pentagonal._2
        val center = pentagonal._1



        val chunk = new ChunkGrid(new OBB(center + up*lUp, right, up, lRight/2, lUp, lLook/2), Engine.getEngine().world)

        Engine.getEngine().debugPlanetLine.addLine(chunk.bound.genRightLine(), vec3(1,0,0))
        Engine.getEngine().debugPlanetLine.addLine(chunk.bound.genUpLine(), vec3(0,1,0))
        Engine.getEngine().debugPlanetLine.addLine(chunk.bound.genLookLine(), vec3(0,0,1))

        val canAdd = canAddChunk(chunk)

        if(canAdd.isEmpty) Breaks.break()

        registerChunk(chunk, canAdd)
      }

      true
    }catchBreak{
      false
    }
  }*/

  /**
    *
    * @param newChunk
    * @return
    *
    * Chunks may intersect !
    * Voxels may not !
    */
  //TODO pretty tough system, testing needed
  def registerChunk(newChunk:ChunkGrid): Boolean =
  {
    if(canAddChunk(newChunk)){
      addChunk(newChunk.center, newChunk)
      true
    }else{
      false
    }

  }


  def canAddChunk(newChunk:ChunkGrid): Boolean =
  {
    !hasChunk(newChunk.center)
  }




  /*def update(pos:vec3, visualExtent:Int): Unit =
  {
    val ps = findAllChunkPositions(pos, visualExtent)
    val random = new Random()
    def init = random.nextFloat() < 0.01F //1% chance


    for(p <- ps){
      if(!hasChunk(p)){ //gen new block
        if(init){
          //val right = vec3(random.nextFloat(), random.nextFloat(), 0).normalize() // ([0,1],[0,1],{0})
          //val up = vec3(0,1,0)
          //val ortho = right ^ up
          val right = vec3(1,0,0)
          val ortho = vec3(0,1,0)


          val chunk = new Chunk(new OBB(p, right, ortho, 8,8,8), this)
          chunk.genSphericalAndMakeConcurrent()
          registerChunk(chunk, Option(vector[vec3](p))) //no checking for extra performance
        }else{
          this.addChunk(p, new Block(p))
        }
      }
    }

  }*/



  def rayTraceChunks(ray:Ray, visualExtent:Int):vector[ChunkGrid] =
  {
    val c = Math.roundTo2Power(ray.start, CVoxel.CHUNK_SIZE_IN_UNITS_POWER)
    val x = c.x.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val y = c.y.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val z = c.z.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt



    val re = new vector[ChunkGrid]()

    for(i:Int <- x-visualExtent to x+visualExtent) for(j:Int  <- y-visualExtent to y+visualExtent) for(k:Int  <- z-visualExtent to z+visualExtent){
      val blockPos = vec3(i*CVoxel.CHUNK_SIZE_IN_UNITS,j*CVoxel.CHUNK_SIZE_IN_UNITS,k*CVoxel.CHUNK_SIZE_IN_UNITS)
      val chunk = getChunk(blockPos)

      if(chunk != null){
        if(CollisionEngine.checkRayBox(ray, new AABB(chunk.center, vec3(chunk.extent,chunk.extent,chunk.extent))).isDefined){
          re += chunk
        }
      }

    }

    re

  }

  def findAllChunkPositions(pos:vec3, visualExtent:Int): vector[vec3] =
  {
    val c = Math.roundTo2Power(pos, CVoxel.CHUNK_SIZE_IN_UNITS_POWER)
    val x = c.x.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val y = c.y.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val z = c.z.toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt


    val re = vector[vec3]()

    for(i:Int <- x-visualExtent to x+visualExtent) for(j:Int  <- y-visualExtent to y+visualExtent) for(k:Int  <- z-visualExtent to z+visualExtent){
      val chunkPos = vec3(i*CVoxel.CHUNK_SIZE_IN_UNITS,j*CVoxel.CHUNK_SIZE_IN_UNITS,k*CVoxel.CHUNK_SIZE_IN_UNITS)

      re += chunkPos

    }

    re
  }

  def findAllChunkPositions(obbg:OBB): vector[vec3] =
  {
    val obb = obbg.scale(0.999F) //to remove collision issues of chunks with equal OBB

    var xmin,xmax,ymin,ymax,zmin,zmax:Float = 0
    var first = true


    val x = vec3(1,0,0)
    val y = vec3(0,1,0)
    val z = vec3(0,0,1)

    val vertices = obb.genVertices()

    for(v <- vertices){
      val p1 = v * x
      val p2 = v * y
      val p3 = v * z

      if(first){
        first = false
        xmin = p1
        xmax  = p1
        ymin = p2
        ymax = p2
        zmin = p3
        zmax = p3
      }else{
        xmin = math.min(xmin, p1)
        xmax = math.max(xmax, p1)

        ymin = math.min(ymin, p2)
        ymax = math.max(ymax, p2)

        zmin = math.min(zmin, p3)
        zmax = math.max(zmax, p3)
      }

    }


    val x1 = Math.roundTo2Power(xmin, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val x2 = Math.roundTo2Power(xmax, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val y1 = Math.roundTo2Power(ymin, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val y2 = Math.roundTo2Power(ymax, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val z1 = Math.roundTo2Power(zmin, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
    val z2 = Math.roundTo2Power(zmax, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt


    val re = new vector[vec3]()


    for(i:Int <- x1 to x2 ) for(j:Int <- y1 to y2 ) for (k:Int<- z1 to z2 ){
      val v = vec3(i*CVoxel.CHUNK_SIZE_IN_UNITS, j * CVoxel.CHUNK_SIZE_IN_UNITS, k * CVoxel.CHUNK_SIZE_IN_UNITS)
      if(CollisionEngine.checkOBBAABB(obb,
        new AABB(v, vec3(CVoxel.CHUNK_SIZE_IN_UNITS/2,CVoxel.CHUNK_SIZE_IN_UNITS/2,CVoxel.CHUNK_SIZE_IN_UNITS/2))) != 0) {
        re += vec3(i * CVoxel.CHUNK_SIZE_IN_UNITS,j * CVoxel.CHUNK_SIZE_IN_UNITS,k * CVoxel.CHUNK_SIZE_IN_UNITS)
      }
    }


    re
  }

  /**
    *TODO SLOW !!!
    * @param obb
    * @param func (vec3 - center of the chunk, Boolean - is the chunk alone)
    */
  def processChunkPositionsWithinOBBIntersection(obb:OBB, @throws(classOf[ControlThrowable]) func: (vec3, Boolean) => Any):Boolean =
  {
    Breaks.tryBreakable
    {
      var xmin,xmax,ymin,ymax,zmin,zmax:Float = 0
      var first = true


      val x = vec3(1,0,0)
      val y = vec3(0,1,0)
      val z = vec3(0,0,1)

      val vertices = obb.genVertices()

      for(v <- vertices){
        val p1 = v * x
        val p2 = v * y
        val p3 = v * z

        if(first){
          first = false
          xmin = p1
          xmax  = p1
          ymin = p2
          ymax = p2
          zmin = p3
          zmax = p3
        }else{
          xmin = math.min(xmin, p1)
          xmax = math.max(xmax, p1)

          ymin = math.min(ymin, p2)
          ymax = math.max(ymax, p2)

          zmin = math.min(zmin, p3)
          zmax = math.max(zmax, p3)
        }

      }


      val x1 = Math.roundTo2Power(xmin, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
      val x2 = Math.roundTo2Power(xmax, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
      val y1 = Math.roundTo2Power(ymin, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
      val y2 = Math.roundTo2Power(ymax, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
      val z1 = Math.roundTo2Power(zmin, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt
      val z2 = Math.roundTo2Power(zmax, CVoxel.CHUNK_SIZE_IN_UNITS_POWER).toInt/CVoxel.CHUNK_SIZE_IN_UNITS.toInt


      val alone = x2-x1 == 0 && y2-y1 == 0 && z2-z1 == 0

      for(i:Int <- x1 to x2) for(j:Int <- y1 to y2) for (k:Int<- z1 to z2){
        val v = vec3(i*CVoxel.CHUNK_SIZE_IN_UNITS, j * CVoxel.CHUNK_SIZE_IN_UNITS, k * CVoxel.CHUNK_SIZE_IN_UNITS)
        if(CollisionEngine.checkOBBAABBSeparatingAxisTheorem(obb,
          new AABB(v, vec3(CVoxel.CHUNK_SIZE_IN_UNITS/2,CVoxel.CHUNK_SIZE_IN_UNITS/2,CVoxel.CHUNK_SIZE_IN_UNITS/2)))) {
          func(vec3(i * CVoxel.CHUNK_SIZE_IN_UNITS,j * CVoxel.CHUNK_SIZE_IN_UNITS,k * CVoxel.CHUNK_SIZE_IN_UNITS), alone)
        }
      }

      true
    }catchBreak
    {
      false
    }
  }

  /**
    *
    * @param obb
    * @param func (Block - chunk, Boolean - is the chunk alone)
    *             chunk may be null !!!
    */
  def processChunksWithinOBBIntersection(obb:OBB, @throws(classOf[ControlThrowable]) func: (ChunkGrid, Boolean) => Any):Unit =
  {
    processChunkPositionsWithinOBBIntersection(obb, (v:vec3, b:Boolean) => func(getChunk(v), b))
  }
}

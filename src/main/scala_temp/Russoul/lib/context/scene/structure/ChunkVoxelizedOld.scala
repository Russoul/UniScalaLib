package Russoul.lib.context.scene.structure

import java.util.concurrent.Executors

import Russoul.engine.core.Engine
import Russoul.lib.common.math.CollisionEngine
import Russoul.lib.common.math.geometry.simple.{OBB, Sphere}
import Russoul.lib.common.math.linear.vec3
import Russoul.lib.common.utils.{ColorUtils, Timer, vector}
import Russoul.lib.context.render.RenderMarchingCubes
import Russoul.lib.context.render.gl.bounds.RenderBox
import Russoul.lib.context.scene.{VOOctree, VoxelGrid}
import Russoul.lib.context.scene.generation.HeightMap
import Russoul.lib.context.scene.structure.sample.{VoxelData, VoxelSample}
import libnoiseforjava.module.Perlin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util
import scala.util.Random


/**
  * Created by Russoul on 19.07.2016.
  */

/**
  *
  * @param bound
  *
  * Chunks may collide !!!
  */
class ChunkVoxelizedOld(val bound:OBB, val world:World)
{


  private val voxels = new VOOctree(bound, ChunkVoxelizedOld.GENERATEDVOXELSIZE, null)
  val listOfVoxels = new vector[Voxel]()

  private val renderVoxels = new RenderBox
  private val renderVoxelsMC  = new RenderMarchingCubes(listOfVoxels, 0)
  private val renderVoxelsPrebuild = new RenderBox       //TODO debug, not needed anymore, can be removed
  private var voxelsPreBuild: vector[VoxelSample] = null //TODO debug, not needed anymore, can be removed

  //NEW TYPE OF GENERATION AND STORAGE - GRID
  //TODO new class
  //.......................................

  def getTree() = voxels

  private def generateTerrainFilled(): Unit =
  {
    val space = ChunkVoxelizedOld.GENERATEDVOXELSIZE

    val numX:Int = (2*bound.extentRight / ChunkVoxelizedOld.GENERATEDVOXELSIZE).toInt
    val numY:Int = (2*bound.extentUp / ChunkVoxelizedOld.GENERATEDVOXELSIZE).toInt
    val numZ:Int = (2*bound.extentLook / ChunkVoxelizedOld.GENERATEDVOXELSIZE).toInt





    val right = bound.right
    val up = bound.up
    val look = right^up

    val er = bound.extentRight
    val eu = bound.extentUp
    val el = bound.extentLook


    val center = bound.center

    val min = bound.genMin()

    val waiting = new vector[VoxelSample]



    val random  = new Random()


    val MULTIPLIER = 0.1F

    Timer.timed("constructing in")
    {

      for (i <- 0 until numX) {
        for (j <- 0 until numY) {
          for (k <- 0 until numZ) {

            val v1 = right * (space / 2 + space * i)
            val v2 = up * (space / 2 + space * j)
            val v3 = look * (space / 2 + space * k)

            val sample = min + v1 + v2 + v3

            val voxelAsBound = new OBB(sample, right, up, space, space, space)
            val vertices = voxelAsBound.genVertices()
            val arrayOfDensities = new Array[Float](8)

            for(i <- 0 until 8 ) arrayOfDensities(i) = world.Generator.genDensity(min + (vertices(i)-min)*MULTIPLIER)





            val im = random.nextFloat() * 2 - 1 //rand
            val sphere = new Sphere(center, er + im)


            if(CollisionEngine.checkPointSphere(sample, sphere)){ //check if voxel is in the sphere

              val color:Int = ColorUtils.genRGB(0,1,0) //TODO
              waiting += new VoxelSample(sample, new VoxelData(arrayOfDensities, color))
            }

          }
        }
      }
    }

    Timer.timed("building of " + waiting.size + " voxels")
    {
      voxels.buildTree(waiting)
    }

    val t = new ArrayBuffer[Int]()


  }

  private def generateTerrainSpherical(): Unit =
  {
    val space:Float = ChunkVoxelizedOld.GENERATEDVOXELSIZE

    val numX:Int = (2*bound.extentRight / ChunkVoxelizedOld.GENERATEDVOXELSIZE).toInt
    val numY:Int = (2*bound.extentUp / ChunkVoxelizedOld.GENERATEDVOXELSIZE).toInt
    val numZ:Int = (2*bound.extentLook / ChunkVoxelizedOld.GENERATEDVOXELSIZE).toInt


    val right = bound.right
    val up = bound.up
    val look = right^up

    val er = bound.extentRight
    val eu = bound.extentUp
    val el = bound.extentLook


    val center = bound.center

    val min = bound.genMin()



    val waiting:vector[VoxelSample] = vector.ofSize(numX*numY*numZ)

    val random  = new Random()
    val MULTIPLIER = 0.01F

    def den(v:vec3):Float =
    {
      val RAD = 3
      val t = (v - center).squareLength() - RAD*RAD
      if(t < 0) -1 else 1

      //t
    }

    def den2(v:vec3):Float =
    {
      if(math.abs(v.x) < 3 && math.abs(v.y - 3) < 3 && math.abs(v.z) < 3) -1 else 1
    }

    import Russoul.lib.common.math.Math._

    for (i <- 0 until numX ) {
      for (j <- 0 until numY ) {
        for (k <- 0 until numZ ) {
          val v1 = right * (space/2 + space * i) //i
          val v2 = up *    (space/2 + space * j)    //j
          val v3 = look *  (space/2 + space * k)  //k

          val pos = min + v1 + v2 + v3

          val voxelAsBound = new OBB(pos, right, up, space/2, space/2, space/2)
          val vertices = voxelAsBound.genVerticesCounterClockwise()
          val arrayOfDensities = new Array[Float](8)

          for(i <- 0 until 8 ) arrayOfDensities(i) = den2(vertices(i))

          val color:Int = ColorUtils.genRGB(255,255,0) //TODO
          val sample = new VoxelSample(pos, new VoxelData(arrayOfDensities, color))
          if(sample.data.nonEmpty(0)){
            waiting += sample
          }

        }
      }
    }

    voxels.buildTree(waiting)

  }

  def genSphericalAndMakeConcurrent():Unit =
  {


    ChunkVoxelizedOld.addThreadedGen(this, c => {
      c.generateTerrainSpherical()
      c.premakeCubic(customColor = true, customColorParam = vec3(0,1,0))
      c.premake()
    })
  }



  private var generatedPrebuild = false
  def premakePrebuild(): Unit = //TODO debug, not needed anymore, can be removed
  {
    for(v <- voxelsPreBuild){
      renderVoxelsPrebuild.add(new OBB(v.point, bound.right, bound.up, 0.01F, 0.01F, 0.01F), vec3(1,1,0))
    }

    generatedPrebuild = true
  }

  private var madePrebuild = false
  def makePrebuild(): Unit =    //TODO debug, not needed anymore, can be removed
  {
    if(generatedPrebuild && !madePrebuild){
      renderVoxelsPrebuild.construct()
      madePrebuild = true
    }
  }

  def renderPrebuild(): Unit =  //TODO debug, not needed anymore, can be removed
  {
    if(madePrebuild) renderVoxelsPrebuild.draw()
  }

  private var generatedCubic = false
  def premakeCubic(customColor :Boolean = true, customColorParam:vec3 = vec3(0,0,0)): Unit =
  {
    getTree().findAllVoxels(listOfVoxels)

    for (v <- listOfVoxels){
      if(v containsSurface 0)//TODO default isolevel is used
        renderVoxels.add(v.bound, if(!customColor)ColorUtils.genRGB(v.data.color) else customColorParam)
    }

    generatedCubic = true //mark chunk as generated, until then no by player modifications are allowed
  }

  private var generated = false
  def premake(): Unit =
  {
    getTree().findAllVoxels(listOfVoxels)

    renderVoxelsMC.preconstruct()

    generated = true //mark chunk as generated, until then no by player modifications are allowed
  }

  private var madeCubic = false
  def makeCubic(): Unit ={
    if(generatedCubic && !madeCubic){
      Timer.timed("make") {
        renderVoxels.construct()
        madeCubic = true
      }
    }
  }


  private var made = false
  def make(): Unit ={
    if(generated && !made){
      Timer.timed("make") {
        renderVoxelsMC.construct()
        made = true
      }
    }
  }

  def renderCubic() = if (madeCubic) {
    renderVoxels.draw()
  }

  def render() = if (made) {
    renderVoxelsMC.draw()
  }


  def deconstruct(): Unit =
  {
    made = false
    renderVoxelsMC.clearPools()
    renderVoxelsMC.deconstruct()
    listOfVoxels.discard()
  }

  def deconstructCubic(): Unit =
  {
    made = false
    renderVoxels.clearPools()
    renderVoxels.deconstruct()
  }

}


object ChunkVoxelizedOld
{
  final val GENERATEDVOXELSIZE:Float = 0.5F



  private val single = new ExecutionContext
  {
    val threadPool = Executors.newSingleThreadExecutor()

    def execute(runnable: Runnable) {
      threadPool.submit(runnable)
    }

    def reportFailure(t: Throwable) {}
  }

  private val fixed = new ExecutionContext
  {
    val threadPool = Executors.newFixedThreadPool(4)

    def execute(runnable: Runnable) {
      threadPool.submit(runnable)
    }

    def reportFailure(t: Throwable) {}
  }


  //Single thread for computing chunks.............

  private var runThread = true
  private final val monitor = new Object

  private val runnable = new Runnable {
    override def run(): Unit =
    {
      //serially construct chunks: one by one
      while(runThread)
      {
        if(chunksToBeConstructedBySingleThread.size > 0)
        {
          val popped = chunksToBeConstructedBySingleThread.pop

          popped._2(popped._1)
        }


        monitor.synchronized
        {
          while(chunksToBeConstructedBySingleThread.size == 0) //while loop to prevent bad stuff as sometimes threads wake up randomly
          {
            try
            {
              monitor.wait() //waiting for monitor notification + chunksToBeConstructedBySingleThread must be > 0
            }catch {
              case e:InterruptedException =>
            }

          }
        }

      }


    }
  }

  private val thread =
  {
    val me = new Thread(runnable)
    me
   /* me.start() //TODO off
    me*/
  }

  private val chunksToBeConstructedBySingleThread = new vector[(ChunkVoxelizedOld,ChunkVoxelizedOld=>Any)]

  //...............................................


  private var serialGenDone = true
  private val serialGenList = new vector[ChunkVoxelizedOld]()

  private def serialPerform(chunk:ChunkVoxelizedOld): Unit =
  {
    implicit val pool = single

    serialGenDone = false
    Future
    {
      chunk.generateTerrainSpherical()
      chunk.premake()
      serialGenDone = true
    }
  }

  private def concurrentPerform(chunk:ChunkVoxelizedOld): Unit =
  {
    implicit val pool = fixed

    Future
    {
      chunk.generateTerrainSpherical()
      chunk.premake()
    }
  }


  def addSerialGen(chunk:ChunkVoxelizedOld): Unit =
  {
    if(serialGenDone){
      serialPerform(chunk)
    }else{
      serialGenList += chunk
    }
  }

  def addConcurrentGen(chunk:ChunkVoxelizedOld): Unit =
  {
    concurrentPerform(chunk)
  }

  def addThreadedGen(chunk:ChunkVoxelizedOld, f:ChunkVoxelizedOld => Any): Unit =
  {



    if(chunksToBeConstructedBySingleThread.size == 0) //notify the thread
    {
      monitor.synchronized
      {
        chunksToBeConstructedBySingleThread += (chunk , f)
        monitor.notifyAll()
      }
    }else{
      chunksToBeConstructedBySingleThread += (chunk ,f)
    }


  }

  def onUpdate(): Unit =
  {
    if(serialGenDone && serialGenList.size > 0){
      val l = serialGenList.pop
      serialPerform(l)
    }
  }

  def releaseGenPools(): Unit =
  {
    single.threadPool.shutdown()
    fixed.threadPool.shutdown()
    thread.stop()
  }

}

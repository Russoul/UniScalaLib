package Russoul.lib.context.scene.structure

import java.util.concurrent.Executors

import Russoul.engine.core.Engine
import Russoul.lib.common.math.CollisionEngine
import Russoul.lib.common.math.immutable.geometry.simple.{AABB, OBB, Ray}
import Russoul.lib.common.math.immutable.linear.vec3
import Russoul.lib.context.render.gl.bounds.RenderBox
import Russoul.lib.context.render.{RenderMarchingCubesGrid, RenderMarchingCubesGridLocked}
import Russoul.lib.context.scene.{CVoxel, VoxelGrid}
import Russoul.lib.common.math.Math._
import Russoul.lib.common.utils.{Timer, vector}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by wzlom on 23.09.2016.
  *
  * Axis aligned cube
  * size/generatedVoxelSize = INTEGER num!
  *
  *
  * Axis Aligned Cube
  */
class ChunkGrid(val center:vec3, val extent:Int, val world:World)
{


  final val grid = new VoxelGrid(center, extent, ChunkGrid.GENERATEDVOXELSIZE,world)
  final val rendererMC = new RenderMarchingCubesGridLocked(grid)
  final val rendererGrid = new RenderBox




  def genSphereAtCam(camPos:vec3): Unit =
  {
    ChunkGrid.ParallelChunkManager.addThreadedGen(this, c => {
      def genFunc(pos:vec3) = (pos - camPos).length() - extent/2//sphere



      c.grid.gen(genFunc)
      c.premake()
      c.generateGrid()
    })
  }


  def genEllipsoid(): Unit =
  {
    val a = 1F
    val b = 1F
    val c = 1F

    ChunkGrid.ParallelChunkManager.addThreadedGen(this, chunk =>
    {
      def genFunc(pos:vec3) =
      {
        val v = (pos.x - chunk.center.x).squared / (a*a) + (pos.y - chunk.center.y).squared / (b*b) + (pos.z-chunk.center.z).squared / (c*c) - 1

        v

      }

      chunk.grid.gen(genFunc)
      chunk.premake()
      chunk.generateGrid()
    })
  }

  def genHyperbolicHyperboloid(): Unit =
  {
    val a = 1/3F
    val b = 1/3F
    val c = 1/3F

    ChunkGrid.ParallelChunkManager.addThreadedGen(this, chunk =>
    {
      def genFunc(pos:vec3) =
      {
        var v = (pos.x - chunk.center.x).squared / (a*a) + (pos.y - chunk.center.y).squared / (b*b) - (pos.z - chunk.center.z).squared / (c*c) - 1

        if ( pos.y < 0) v = 1

        v

      }

      chunk.grid.gen(genFunc)
      chunk.premake()
      chunk.generateGrid()

    })
  }

  private var generated = false
  private var made = false

  def isMade() = made
  def isGenerated() = generated


  /**
    * heavy
    */
  def rebuild(): Unit =
  {
    generated = false
    made = false

    rendererMC.deconstruct()
    rendererMC.clearPools()


    ChunkGrid.ParallelChunkManager.addThreadedGen(this , _=> {
      premake()
    })
  }

  def rebuildGrid(): Unit =
  {

    madeGrid = false
    generatedGrid = false
    rendererGrid.deconstruct()
    rendererGrid.clearPools()

    ChunkGrid.ParallelChunkManager.addThreadedGen(this , _=>
    {
      generateGrid(true) //forced as generated might not be true at the moment, but density field is present
    })
  }



  def premake(): Unit =
  {
    rendererMC.preconstruct()

    generated = true
  }

  def make(): Unit =
  {
    if(generated && !made){
      rendererMC.construct()
      made = true
    }
  }

  def render(): Unit =
  {
    if(made) rendererMC.draw()
  }


  private var generatedGrid = false
  private var madeGrid = false

  def generateGrid(force:Boolean = false): Unit =
  {
    if( (generated || force) && !generatedGrid){
      val size = 0.005F

      for(i <- 0 until grid.densitiesOnSide()){
        for(j <- 0 until grid.densitiesOnSide()){
          for(k <- 0 until grid.densitiesOnSide()){

            var p = grid.densityAt(i,j,k)

            if(grid.density(i,j,k) <= 0){
              rendererGrid.add(new AABB(p, vec3(size,size, size)), vec3(1,1,0))

              //render nearest
              if(i > 0 && grid.density(i-1,j,k) > 0){
                p = grid.densityAt(i-1,j,k)
                rendererGrid.add(new AABB(p, vec3(size,size, size)), vec3(1,1,1))
              }
              if(i < grid.densitiesOnSide()-1 && grid.density(i+1,j,k) > 0){
                p = grid.densityAt(i+1,j,k)
                rendererGrid.add(new AABB(p, vec3(size,size, size)), vec3(1,1,1))
              }

              if(j > 0 && grid.density(i,j-1,k) > 0){
                p = grid.densityAt(i,j-1,k)
                rendererGrid.add(new AABB(p, vec3(size,size, size)), vec3(1,1,1))
              }
              if(j < grid.densitiesOnSide()-1 && grid.density(i,j+1,k) > 0){
                p = grid.densityAt(i,j+1,k)
                rendererGrid.add(new AABB(p, vec3(size,size, size)), vec3(1,1,1))
              }

              if(k > 0 && grid.density(i,j,k-1) > 0){
                p = grid.densityAt(i,j,k-1)
                rendererGrid.add(new AABB(p, vec3(size,size, size)), vec3(1,1,1))
              }
              if(k < grid.densitiesOnSide()-1 && grid.density(i,j,k+1) > 0){
                p = grid.densityAt(i,j,k+1)
                rendererGrid.add(new AABB(p, vec3(size,size, size)), vec3(1,1,1))
              }
            }

          }
        }
      }

      generatedGrid = true
    }
  }
  def makeGrid(): Unit =
  {
    if(!madeGrid && generatedGrid){
      rendererGrid.construct()
      madeGrid = true
    }
  }

  def renderGrid(): Unit =
  {
    if(madeGrid) rendererGrid.draw()
  }


}


object ChunkGrid
{
  final val GENERATEDVOXELSIZE:Float = 1/5F


  //Threads for creating and computing chunks.............
  object ParallelChunkManager
  {
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

            popped._2(popped._1) //call operation
          }


          monitor.synchronized
          {
            while(chunksToBeConstructedBySingleThread.size == 0 && runThread) //while loop to prevent bad stuff as sometimes threads wake up randomly
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
      new Thread(runnable)
    }



    private val chunksToBeConstructedBySingleThread = new vector[(ChunkGrid,ChunkGrid=>Any)]



    private var runThreadCreator = true
    private final val monitorCreator = new Object

    private val runnableCreator = new Runnable {
      override def run(): Unit =
      {
        //serially construct chunks: one by one
        while(runThreadCreator)
        {
          if(chunksToBeCreated.size > 0)
          {
            val popped = chunksToBeCreated.pop

            val c = new ChunkGrid(popped._1, CVoxel.CHUNK_SIZE_IN_UNITS.toInt/2, Engine.getEngine().world)

            Engine.getEngine().world.registerChunk(c)

            popped._2.apply(c) //call callback func
          }


          monitorCreator.synchronized
          {
            while(chunksToBeCreated.size == 0 && runThreadCreator) //while loop to prevent bad stuff as sometimes threads wake up randomly
            {
              try
              {
                monitorCreator.wait() //waiting for monitor notification + chunksToBeConstructedBySingleThread must be > 0
              }catch {
                case e:InterruptedException =>
              }

            }
          }

        }


      }
    }

    private val threadCreator =
    {
      new Thread(runnableCreator)
    }

    private val chunksToBeCreated = vector[(vec3,ChunkGrid=>Any)]()



    def runService() =
    {
      thread.start()
      threadCreator.start()
    }

    def stopService() =
    {
      runThread = false //stop thread that computes chunks
      monitor.synchronized
      {
        monitor.notifyAll() //notify it
      }


      runThreadCreator = false //stop thread that initializes chunks
      monitorCreator.synchronized
      {
        monitorCreator.notifyAll() //notify it
      }
    }




    def addThreadedGen(chunk:ChunkGrid, f:ChunkGrid => Any): Unit =
    {



      if(ParallelChunkManager.chunksToBeConstructedBySingleThread.size == 0) //notify the thread
      {
        ParallelChunkManager.monitor.synchronized
        {
          ParallelChunkManager.chunksToBeConstructedBySingleThread += (chunk , f)
          ParallelChunkManager.monitor.notifyAll()
        }
      }else{
        ParallelChunkManager.chunksToBeConstructedBySingleThread += (chunk ,f)
      }


    }

    def registerNewChunk(pos:vec3, callback:(ChunkGrid) => Any):Unit =
    {
      for(v <- ParallelChunkManager.chunksToBeCreated){ //check if chunks with the same pos are to be registered
        if(v._1 == pos) return
      }

      if(ParallelChunkManager.chunksToBeCreated.size == 0) //notify the thread
      {
        ParallelChunkManager.monitorCreator.synchronized
        {
          ParallelChunkManager.chunksToBeCreated += (pos,callback)
          ParallelChunkManager.monitorCreator.notifyAll()
        }
      }else{
        ParallelChunkManager.chunksToBeCreated += (pos,callback)
      }
    }

  }



  //...............................................







}

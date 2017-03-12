package Russoul.lib.context.render

import Russoul.lib.common.math.Math
import Russoul.lib.common.math.immutable.linear.vec3
import Russoul.lib.common.utils.{ColorUtils, vector}
import Russoul.lib.context.scene.VoxelGrid
import Russoul.lib.context.scene.structure.ChunkGrid
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.{GL11, GL20}

import scala.language.postfixOps

/**
  * Created by wzlom on 21.08.2016.
  */
class RenderMarchingCubesGridLocked(private val grid: VoxelGrid, private val isoLevel:Float = 0)
{
  private final val VERTEX_SIZE: Int = 9


  private final val MAX_VERTICES_PER_VOXEL = 15
  private val vertexPool = new vector[Float]()  //dynamic growth
  private val indexPool = vector.ofSize[Int](grid.voxelCount()*MAX_VERTICES_PER_VOXEL) //static,global
  private val actualIndexPool = vector[Int]()
  private var constructed = false

  var VBO,VAO,EBO:Int = 0


  { //TODO this is needed, make it faster and look nicer
    for(i <- indexPool.indices){
      indexPool(i) = -1
    }
  }

  //TODO test rebuild and build


  def rebuildVoxel(iii:(Int,Int,Int)): Unit =
  {
    import ColorUtils.SmartVec3ToRGB

    var offset = 0

    val vos = grid.voxelsOnSide()
    val tempVertices = vector[vec3](12)

    /**
      *
      * @param center
      * @param extent
      * @param voxelSize
      * @param r
      * @param u
      * @param l
      *
      *          emulates same function in VoxelGrid but without a need of a new class instance
      */
    @inline def densityAt(center:vec3, extent:Float, voxelSize:Float, r:Int, u:Int, l:Int): vec3 =
    {
      center - vec3(extent,extent,extent) + vec3(r*voxelSize+voxelSize/2, u*voxelSize+voxelSize/2, l*voxelSize+voxelSize/2)
    }

    @inline def addVertex(i:Int, p:vec3) =
    {
      tempVertices(i) = p

    }


    @inline def operate(dens:Array[Float], vertices:Array[vec3], colors:Array[Int], iii:(Int,Int,Int)): Unit =
    {
      var casee:Int = 0

      if(dens(0) < isoLevel) casee = casee | Math.power(2, 0)
      if(dens(1) < isoLevel) casee = casee | Math.power(2, 1)
      if(dens(2) < isoLevel) casee = casee | Math.power(2, 2)
      if(dens(3) < isoLevel) casee = casee | Math.power(2, 3)

      if(dens(4) < isoLevel) casee = casee | Math.power(2, 4)
      if(dens(5) < isoLevel) casee = casee | Math.power(2, 5)
      if(dens(6) < isoLevel) casee = casee | Math.power(2, 6)
      if(dens(7) < isoLevel) casee = casee | Math.power(2, 7)



      var deletions = 0
      for(i <- 0 until MAX_VERTICES_PER_VOXEL) {
        val k = (iii._1 * vos * vos + iii._2 * vos + iii._3) * MAX_VERTICES_PER_VOXEL + i
        if (indexPool(k) != -1){
          vertexPool.remove(indexPool(k),VERTEX_SIZE)
          indexPool(k) = -1
          deletions +=1
        }
      }

      for(i <- indexPool.indices)
      {
        if(indexPool(i) != -1) indexPool(i) -= deletions
      }



      val edges = RenderMarchingCubesGridLocked.edgeTable(casee)




      if(edges > 0){ //else - no intersection => no triangles generated



        var numOfVertices = 0

        def addVertexGlobal(p:vec3, color:vec3): Unit =
        {
          vertexPool += p.x
          vertexPool += p.y
          vertexPool += p.z

          vertexPool += color.x
          vertexPool += color.y
          vertexPool += color.z

          vertexPool += 0 //save space for normal
          vertexPool += 0
          vertexPool += 0

          numOfVertices += 1

          if(numOfVertices % 3 == 0) //we add normal to triangle data,as triangle is now defined
          {

            val x1 = vertexPool(vertexPool.size-9)
            val y1 = vertexPool(vertexPool.size-8) //pos1
          val z1 = vertexPool(vertexPool.size-7)

            val x2 = vertexPool(vertexPool.size-18)
            val y2 = vertexPool(vertexPool.size-17) //pos2
          val z2 = vertexPool(vertexPool.size-16)

            val x3 = vertexPool(vertexPool.size-27)
            val y3 = vertexPool(vertexPool.size-26) //pos3 of last triangle
          val z3 = vertexPool(vertexPool.size-25)

            val v1 = vec3(x1,y1,z1)
            val v2 = vec3(x2,y2,z2)
            val v3 = vec3(x3,y3,z3)

            val normal = -( (v3-v2)^(v1-v2) ).normalize() //gen normal

            vertexPool(vertexPool.size - 3) = normal.x
            vertexPool(vertexPool.size - 2) = normal.y //set normal for each vertex
            vertexPool(vertexPool.size - 1) = normal.z

            vertexPool(vertexPool.size - 12) = normal.x
            vertexPool(vertexPool.size - 11) = normal.y
            vertexPool(vertexPool.size - 10) = normal.z

            vertexPool(vertexPool.size - 21) = normal.x
            vertexPool(vertexPool.size - 20) = normal.y
            vertexPool(vertexPool.size - 19) = normal.z
          }
        }



        if((edges & 1) != 0){
          addVertex(0,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(0), vertices(1), dens(0), dens(1)))
        }
        if((edges & 2) != 0){
          addVertex(1,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(1), vertices(2), dens(1), dens(2)))
        }
        if((edges & 4) != 0){
          addVertex(2,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(2), vertices(3), dens(2), dens(3)))
        }
        if((edges & 8) != 0){
          addVertex(3,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(3), vertices(0), dens(3), dens(0)))
        }
        if((edges & 16) != 0){
          addVertex(4,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(4), vertices(5), dens(4), dens(5)))
        }
        if((edges & 32) != 0){
          addVertex(5, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(5), vertices(6), dens(5), dens(6)))
        }
        if((edges & 64) != 0){
          addVertex(6,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(6), vertices(7), dens(6), dens(7)))
        }
        if((edges & 128) != 0){
          addVertex(7, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(7), vertices(4), dens(7), dens(4)))
        }
        if((edges & 256) != 0){
          addVertex(8, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(0), vertices(4), dens(0), dens(4)))
        }
        if((edges & 512) != 0){
          addVertex(9, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(1), vertices(5), dens(1), dens(5)))
        }
        if((edges & 1024) != 0){
          addVertex(10, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(2), vertices(6), dens(2), dens(6)))
        }
        if((edges & 2048) != 0){
          addVertex(11, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(3), vertices(7), dens(3), dens(7)))
        }


        var i = 0//its guaranteed that their will be at least 3 iterations (1 triangle)
        var cur = 0
        def get(casee:Int,i:Int) =
        {
          cur = RenderMarchingCubesGridLocked.getTri(casee, i)

          cur
        }

        while(get(casee, i) != -1)
        {
          addVertexGlobal(tempVertices(cur), colors(0) genRGB())


          indexPool((iii._1*vos*vos+iii._2*vos+iii._3)*MAX_VERTICES_PER_VOXEL + i) = offset + i

          i+=1
        }

        offset += numOfVertices
        tempVertices.discard()
      }
    }

    val vertices = new Array[vec3](8)
    val colors = new Array[Int](8)
    val dens = new Array[Float](8)


    val x = iii._1
    val y = iii._2
    val z = iii._3

    vertices(0) = grid.densityAt(x,y,z)
    vertices(1) = grid.densityAt(x+1,y,z)
    vertices(2) = grid.densityAt(x+1,y,z+1)
    vertices(3) = grid.densityAt(x,y,z+1)

    vertices(4) = grid.densityAt(x,y+1,z)
    vertices(5) = grid.densityAt(x+1,y+1,z)
    vertices(6) = grid.densityAt(x+1,y+1,z+1)
    vertices(7) = grid.densityAt(x,y+1,z+1)


    colors(0) = grid.color(x,y,z)
    colors(1) = grid.color(x+1,y,z)
    colors(2) = grid.color(x+1,y,z+1)
    colors(3) = grid.color(x,y,z+1)

    colors(4) = grid.color(x,y+1,z)
    colors(5) = grid.color(x+1,y+1,z)
    colors(6) = grid.color(x+1,y+1,z+1)
    colors(7) = grid.color(x,y+1,z+1)


    dens(0) = grid.density(x,y,z)
    dens(1) = grid.density(x+1,y,z)
    dens(2) = grid.density(x+1,y,z+1)
    dens(3) = grid.density(x,y,z+1)

    dens(4) = grid.density(x,y+1,z)
    dens(5) = grid.density(x+1,y+1,z)
    dens(6) = grid.density(x+1,y+1,z+1)
    dens(7) = grid.density(x,y+1,z+1)

    operate(dens,vertices,colors, (x+1,y+1,z+1))

    indexPool.equaliseSizes() //don't forget this !
    actualIndexPool.discard()

    for(i <- indexPool.indices)
    {
      if(indexPool(i) != -1) actualIndexPool += indexPool(i)
    }
  }


  def preconstruct(): Unit =
  {
    import ColorUtils.SmartVec3ToRGB

    var offset = 0

    val vos = grid.voxelsOnSide()
    val tempVertices = vector[vec3](12)

    /**
      *
      * @param center
      * @param extent
      * @param voxelSize
      * @param r
      * @param u
      * @param l
      *
      *          emulates same function in VoxelGrid but without a need of a new class instance
      */
    @inline def densityAt(center:vec3, extent:Float, voxelSize:Float, r:Int, u:Int, l:Int): vec3 =
    {
      center - vec3(extent,extent,extent) + vec3(r*voxelSize+voxelSize/2, u*voxelSize+voxelSize/2, l*voxelSize+voxelSize/2)
    }

    @inline def addVertex(i:Int, p:vec3) =
    {
      tempVertices(i) = p

    }
    
    @inline def operate(dens:Array[Float], vertices:Array[vec3], colors:Array[Int], iii:(Int,Int,Int)): Unit =
    {
      var casee:Int = 0
      
      if(dens(0) < isoLevel) casee = casee | Math.power(2, 0)
      if(dens(1) < isoLevel) casee = casee | Math.power(2, 1)
      if(dens(2) < isoLevel) casee = casee | Math.power(2, 2)
      if(dens(3) < isoLevel) casee = casee | Math.power(2, 3)

      if(dens(4) < isoLevel) casee = casee | Math.power(2, 4)
      if(dens(5) < isoLevel) casee = casee | Math.power(2, 5)
      if(dens(6) < isoLevel) casee = casee | Math.power(2, 6)
      if(dens(7) < isoLevel) casee = casee | Math.power(2, 7)




      val edges = RenderMarchingCubesGridLocked.edgeTable(casee)




      if(edges > 0){ //else - no intersection => no triangles generated

        var numOfVertices = 0

        def addVertexGlobal(p:vec3, color:vec3): Unit =
        {
          vertexPool += p.x
          vertexPool += p.y
          vertexPool += p.z

          vertexPool += color.x
          vertexPool += color.y
          vertexPool += color.z

          vertexPool += 0 //save space for normal
          vertexPool += 0
          vertexPool += 0

          numOfVertices += 1

          if(numOfVertices % 3 == 0) //we add normal to triangle data,as triangle is now defined
          {

            val x1 = vertexPool(vertexPool.size-9)
            val y1 = vertexPool(vertexPool.size-8) //pos1
            val z1 = vertexPool(vertexPool.size-7)

            val x2 = vertexPool(vertexPool.size-18)
            val y2 = vertexPool(vertexPool.size-17) //pos2
            val z2 = vertexPool(vertexPool.size-16)

            val x3 = vertexPool(vertexPool.size-27)
            val y3 = vertexPool(vertexPool.size-26) //pos3 of last triangle
            val z3 = vertexPool(vertexPool.size-25)

            val v1 = vec3(x1,y1,z1)
            val v2 = vec3(x2,y2,z2)
            val v3 = vec3(x3,y3,z3)

            val normal = -( (v3-v2)^(v1-v2) ).normalize() //gen normal

            vertexPool(vertexPool.size - 3) = normal.x
            vertexPool(vertexPool.size - 2) = normal.y //set normal for each vertex
            vertexPool(vertexPool.size - 1) = normal.z

            vertexPool(vertexPool.size - 12) = normal.x
            vertexPool(vertexPool.size - 11) = normal.y
            vertexPool(vertexPool.size - 10) = normal.z

            vertexPool(vertexPool.size - 21) = normal.x
            vertexPool(vertexPool.size - 20) = normal.y
            vertexPool(vertexPool.size - 19) = normal.z
          }
        }



        if((edges & 1) != 0){
          addVertex(0,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(0), vertices(1), dens(0), dens(1)))
        }
        if((edges & 2) != 0){
          addVertex(1,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(1), vertices(2), dens(1), dens(2)))
        }
        if((edges & 4) != 0){
          addVertex(2,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(2), vertices(3), dens(2), dens(3)))
        }
        if((edges & 8) != 0){
          addVertex(3,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(3), vertices(0), dens(3), dens(0)))
        }
        if((edges & 16) != 0){
          addVertex(4,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(4), vertices(5), dens(4), dens(5)))
        }
        if((edges & 32) != 0){
          addVertex(5, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(5), vertices(6), dens(5), dens(6)))
        }
        if((edges & 64) != 0){
          addVertex(6,RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(6), vertices(7), dens(6), dens(7)))
        }
        if((edges & 128) != 0){
          addVertex(7, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(7), vertices(4), dens(7), dens(4)))
        }
        if((edges & 256) != 0){
          addVertex(8, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(0), vertices(4), dens(0), dens(4)))
        }
        if((edges & 512) != 0){
          addVertex(9, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(1), vertices(5), dens(1), dens(5)))
        }
        if((edges & 1024) != 0){
          addVertex(10, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(2), vertices(6), dens(2), dens(6)))
        }
        if((edges & 2048) != 0){
          addVertex(11, RenderMarchingCubesGridLocked.linearInterp(isoLevel, vertices(3), vertices(7), dens(3), dens(7)))
        }


        var i = 0//its guaranteed that their will be at least 3 iterations (1 triangle)
        var cur = 0
        def get(casee:Int,i:Int) =
        {
          cur = RenderMarchingCubesGridLocked.getTri(casee, i)

          cur
        }

        while(get(casee, i) != -1)
        {
          addVertexGlobal(tempVertices(cur), colors(0) genRGB())


          indexPool((iii._1*vos*vos+iii._2*vos+iii._3)*MAX_VERTICES_PER_VOXEL + i) = offset + i
          actualIndexPool += offset + i

          i+=1
        }

        offset += numOfVertices
        tempVertices.discard()
      }
    }

    val vertices = new Array[vec3](8)
    val colors = new Array[Int](8)
    val dens = new Array[Float](8)



    for(z <- 0 until grid.voxelsOnSide()) { //YOZ
      for (y <- 0 until grid.voxelsOnSide()) {
        val stuff011 = grid.getChunksVoxelIntersects(0, y, z)
        var i = 0
        for (i <- 0 until 8) {
          val st = stuff011(i)
          vertices(i) = densityAt(st._1, grid.extent, ChunkGrid.GENERATEDVOXELSIZE, st._2._1, st._2._2, st._2._3)
          if (!grid.world.hasChunk(st._1)) {
            dens(i) = 1
            colors(i) = 0
          } else {
            val chunk = grid.world.getChunk(st._1)
            dens(i) = chunk.grid.density(st._2._1, st._2._2, st._2._3)
            colors(i) = chunk.grid.color(st._2._1, st._2._2, st._2._3)
          }
        }
        operate(dens,vertices,colors, (0,y,z))
      }
    }

    for(x <- 1 until grid.voxelsOnSide()) { //XOY
      for (y <- 0 until grid.voxelsOnSide()) {
        val stuff011 = grid.getChunksVoxelIntersects(x, y, 0)
        for (i <- 0 until 8) {
          val st = stuff011(i)
          vertices(i) = densityAt(st._1, grid.extent, ChunkGrid.GENERATEDVOXELSIZE, st._2._1, st._2._2, st._2._3)
          if (!grid.world.hasChunk(st._1)) {
            dens(i) = 1
            colors(i) = 0
          } else {
            val chunk = grid.world.getChunk(st._1)
            dens(i) = chunk.grid.density(st._2._1, st._2._2, st._2._3)
            colors(i) = chunk.grid.color(st._2._1, st._2._2, st._2._3)
          }
        }
        operate(dens,vertices,colors, (x,y,0))
      }
    }

    for(x <- 1 until grid.voxelsOnSide()) { //XOZ
      for (z <- 1 until grid.voxelsOnSide()) {
        val stuff011 = grid.getChunksVoxelIntersects(x, 0, z)
        var i = 0
        for (i <- 0 until 8) {
          val st = stuff011(i)
          vertices(i) = densityAt(st._1, grid.extent, ChunkGrid.GENERATEDVOXELSIZE, st._2._1, st._2._2, st._2._3)
          if (!grid.world.hasChunk(st._1)) {
            dens(i) = 1
            colors(i) = 0
          } else {
            val chunk = grid.world.getChunk(st._1)
            dens(i) = chunk.grid.density(st._2._1, st._2._2, st._2._3)
            colors(i) = chunk.grid.color(st._2._1, st._2._2, st._2._3)
          }
        }
        operate(dens,vertices,colors, (x,0,z))
      }
    }
    
    

    for(x <- 0 until grid.densitiesOnSide()    -1){
      for(y <- 0 until grid.densitiesOnSide()  -1){
        for(z <- 0 until grid.densitiesOnSide()-1){

          
          vertices(0) = grid.densityAt(x,y,z)
          vertices(1) = grid.densityAt(x+1,y,z)
          vertices(2) = grid.densityAt(x+1,y,z+1)
          vertices(3) = grid.densityAt(x,y,z+1)

          vertices(4) = grid.densityAt(x,y+1,z)
          vertices(5) = grid.densityAt(x+1,y+1,z)
          vertices(6) = grid.densityAt(x+1,y+1,z+1)
          vertices(7) = grid.densityAt(x,y+1,z+1)

          
          colors(0) = grid.color(x,y,z) 
          colors(1) = grid.color(x+1,y,z) 
          colors(2) = grid.color(x+1,y,z+1) 
          colors(3) = grid.color(x,y,z+1) 

          colors(4) = grid.color(x,y+1,z) 
          colors(5) = grid.color(x+1,y+1,z) 
          colors(6) = grid.color(x+1,y+1,z+1) 
          colors(7) = grid.color(x,y+1,z+1) 

          
          dens(0) = grid.density(x,y,z)
          dens(1) = grid.density(x+1,y,z)
          dens(2) = grid.density(x+1,y,z+1)
          dens(3) = grid.density(x,y,z+1)

          dens(4) = grid.density(x,y+1,z)
          dens(5) = grid.density(x+1,y+1,z)
          dens(6) = grid.density(x+1,y+1,z+1)
          dens(7) = grid.density(x,y+1,z+1)

          operate(dens,vertices,colors, (x+1,y+1,z+1))
          
        }
      }


    }

    indexPool.equaliseSizes() //don't forget this !


    //if + inside - outside => reverse needed
    //indexPool = indexPool.reverse //have to do this as indices in the table construct triangles in clockwise order but counter-clockwise is needed
  }

  def construct():Boolean =
  {
    if(constructed) return false

    VAO = glGenVertexArrays()
    VBO = glGenBuffers()
    EBO = glGenBuffers()

    glBindVertexArray(VAO)


    glBindBuffer(GL_ARRAY_BUFFER, VBO)
    glBufferData(GL_ARRAY_BUFFER, vertexPool.array, GL_STATIC_DRAW)


    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO)
    glBufferData(GL_ELEMENT_ARRAY_BUFFER,actualIndexPool.array, GL_STATIC_DRAW)


    GL20.glVertexAttribPointer(0, 3, GL11.GL_FLOAT, false, VERTEX_SIZE * 4, 0)
    GL20.glEnableVertexAttribArray(0)

    GL20.glVertexAttribPointer(1, 3, GL11.GL_FLOAT, false, VERTEX_SIZE * 4, 3 * 4)
    GL20.glEnableVertexAttribArray(1)

    GL20.glVertexAttribPointer(2, 3, GL11.GL_FLOAT, false, VERTEX_SIZE * 4, 6 * 4)
    GL20.glEnableVertexAttribArray(2)



    glBindBuffer(GL_ARRAY_BUFFER, 0); // Note that this is allowed, the call to glVertexAttribPointer registered VBO as the currently bound vertex buffer object so afterwards we can safely unbind

    glBindVertexArray(0); // Unbind VAO (it's always a good thing to unbind any buffer/array to prevent strange bugs), remember: do NOT unbind the EBO, keep it bound to this VAO


    //actualIndexPool.clear()


    constructed = true
    true
  }


  def clearPools() =
  {
    vertexPool.discard()
    indexPool.discard()
  }

  def getVertices() = vertexPool

  def getIndices() = indexPool

  def draw(lines:Boolean = false): Unit ={
    if(constructed){
      glBindVertexArray(VAO)
      if(lines)GL11.glPolygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_LINE)
      GL11.glDrawElements(GL11.GL_TRIANGLES, actualIndexPool.size, GL11.GL_UNSIGNED_INT, 0)
      if(lines)GL11.glPolygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL)
      glBindVertexArray(0)
    }
  }

  def deconstruct(): Boolean ={
    if(!constructed) return false

    glDeleteVertexArrays(VAO)
    glDeleteBuffers(VBO)
    glDeleteBuffers(EBO)

    constructed = false
    true
  }
}

/**
  * http://paulbourke.net/geometry/polygonise/
  */
object RenderMarchingCubesGridLocked
{
  def linearInterp1(value:Float, pg1:vec3,pg2:vec3, val1:Float, val2:Float):vec3 =
  {


    var p1:vec3 = pg1
    var p2:vec3 = pg2

    if (p2 < p1)
    {
      var temp:vec3 = null
      temp = p1
      p1 = p2
      p2 = temp
    }




    if(math.abs(val1 - val2) > 0.00001)
      p1 = p1 + (p2 - p1)/(val2 - val1)*(value - val1)


     p1
  }

  def linearInterp(value:Float, pg1:vec3,pg2:vec3, val1:Float, val2:Float): vec3 =
  {
    if(val2 > val1){
      return pg1 + (pg2-pg1)*(-val1/(-val1 + val2))
    }else{
      return pg2 + (pg1-pg2)*(-val2/(-val2 + val1))
    }
  }



  private final val edgeTable:Array[Int] = Array[Int](
    0x0  , 0x109, 0x203, 0x30a, 0x406, 0x50f, 0x605, 0x70c,
    0x80c, 0x905, 0xa0f, 0xb06, 0xc0a, 0xd03, 0xe09, 0xf00,
    0x190, 0x99 , 0x393, 0x29a, 0x596, 0x49f, 0x795, 0x69c,
    0x99c, 0x895, 0xb9f, 0xa96, 0xd9a, 0xc93, 0xf99, 0xe90,
    0x230, 0x339, 0x33 , 0x13a, 0x636, 0x73f, 0x435, 0x53c,
    0xa3c, 0xb35, 0x83f, 0x936, 0xe3a, 0xf33, 0xc39, 0xd30,
    0x3a0, 0x2a9, 0x1a3, 0xaa , 0x7a6, 0x6af, 0x5a5, 0x4ac,
    0xbac, 0xaa5, 0x9af, 0x8a6, 0xfaa, 0xea3, 0xda9, 0xca0,
    0x460, 0x569, 0x663, 0x76a, 0x66 , 0x16f, 0x265, 0x36c,
    0xc6c, 0xd65, 0xe6f, 0xf66, 0x86a, 0x963, 0xa69, 0xb60,
    0x5f0, 0x4f9, 0x7f3, 0x6fa, 0x1f6, 0xff , 0x3f5, 0x2fc,
    0xdfc, 0xcf5, 0xfff, 0xef6, 0x9fa, 0x8f3, 0xbf9, 0xaf0,
    0x650, 0x759, 0x453, 0x55a, 0x256, 0x35f, 0x55 , 0x15c,
    0xe5c, 0xf55, 0xc5f, 0xd56, 0xa5a, 0xb53, 0x859, 0x950,
    0x7c0, 0x6c9, 0x5c3, 0x4ca, 0x3c6, 0x2cf, 0x1c5, 0xcc ,
    0xfcc, 0xec5, 0xdcf, 0xcc6, 0xbca, 0xac3, 0x9c9, 0x8c0,
    0x8c0, 0x9c9, 0xac3, 0xbca, 0xcc6, 0xdcf, 0xec5, 0xfcc,
    0xcc , 0x1c5, 0x2cf, 0x3c6, 0x4ca, 0x5c3, 0x6c9, 0x7c0,
    0x950, 0x859, 0xb53, 0xa5a, 0xd56, 0xc5f, 0xf55, 0xe5c,
    0x15c, 0x55 , 0x35f, 0x256, 0x55a, 0x453, 0x759, 0x650,
    0xaf0, 0xbf9, 0x8f3, 0x9fa, 0xef6, 0xfff, 0xcf5, 0xdfc,
    0x2fc, 0x3f5, 0xff , 0x1f6, 0x6fa, 0x7f3, 0x4f9, 0x5f0,
    0xb60, 0xa69, 0x963, 0x86a, 0xf66, 0xe6f, 0xd65, 0xc6c,
    0x36c, 0x265, 0x16f, 0x66 , 0x76a, 0x663, 0x569, 0x460,
    0xca0, 0xda9, 0xea3, 0xfaa, 0x8a6, 0x9af, 0xaa5, 0xbac,
    0x4ac, 0x5a5, 0x6af, 0x7a6, 0xaa , 0x1a3, 0x2a9, 0x3a0,
    0xd30, 0xc39, 0xf33, 0xe3a, 0x936, 0x83f, 0xb35, 0xa3c,
    0x53c, 0x435, 0x73f, 0x636, 0x13a, 0x33 , 0x339, 0x230,
    0xe90, 0xf99, 0xc93, 0xd9a, 0xa96, 0xb9f, 0x895, 0x99c,
    0x69c, 0x795, 0x49f, 0x596, 0x29a, 0x393, 0x99 , 0x190,
    0xf00, 0xe09, 0xd03, 0xc0a, 0xb06, 0xa0f, 0x905, 0x80c,
    0x70c, 0x605, 0x50f, 0x406, 0x30a, 0x203, 0x109, 0x0   )



  def getTri(casee:Int, num:Int) = triTable(casee*16 + num)

  private final val triTable:Array[Int] = Array[Int](
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 8, 3, 9, 8, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 3, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, 2, 10, 0, 2, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    2, 8, 3, 2, 10, 8, 10, 9, 8, -1, -1, -1, -1, -1, -1, -1,
    3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 11, 2, 8, 11, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 9, 0, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 11, 2, 1, 9, 11, 9, 8, 11, -1, -1, -1, -1, -1, -1, -1,
    3, 10, 1, 11, 10, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 10, 1, 0, 8, 10, 8, 11, 10, -1, -1, -1, -1, -1, -1, -1,
    3, 9, 0, 3, 11, 9, 11, 10, 9, -1, -1, -1, -1, -1, -1, -1,
    9, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 3, 0, 7, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 1, 9, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 1, 9, 4, 7, 1, 7, 3, 1, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 10, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    3, 4, 7, 3, 0, 4, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1,
    9, 2, 10, 9, 0, 2, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1,
    2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, -1, -1, -1, -1,
    8, 4, 7, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    11, 4, 7, 11, 2, 4, 2, 0, 4, -1, -1, -1, -1, -1, -1, -1,
    9, 0, 1, 8, 4, 7, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1,
    4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1, -1, -1, -1, -1,
    3, 10, 1, 3, 11, 10, 7, 8, 4, -1, -1, -1, -1, -1, -1, -1,
    1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4, -1, -1, -1, -1,
    4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3, -1, -1, -1, -1,
    4, 7, 11, 4, 11, 9, 9, 11, 10, -1, -1, -1, -1, -1, -1, -1,
    9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, 5, 4, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 5, 4, 1, 5, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    8, 5, 4, 8, 3, 5, 3, 1, 5, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 10, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    3, 0, 8, 1, 2, 10, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1,
    5, 2, 10, 5, 4, 2, 4, 0, 2, -1, -1, -1, -1, -1, -1, -1,
    2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, -1, -1, -1, -1,
    9, 5, 4, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 11, 2, 0, 8, 11, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1,
    0, 5, 4, 0, 1, 5, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1,
    2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5, -1, -1, -1, -1,
    10, 3, 11, 10, 1, 3, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1,
    4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10, -1, -1, -1, -1,
    5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3, -1, -1, -1, -1,
    5, 4, 8, 5, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1,
    9, 7, 8, 5, 7, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, 3, 0, 9, 5, 3, 5, 7, 3, -1, -1, -1, -1, -1, -1, -1,
    0, 7, 8, 0, 1, 7, 1, 5, 7, -1, -1, -1, -1, -1, -1, -1,
    1, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, 7, 8, 9, 5, 7, 10, 1, 2, -1, -1, -1, -1, -1, -1, -1,
    10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, -1, -1, -1, -1,
    8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2, -1, -1, -1, -1,
    2, 10, 5, 2, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1,
    7, 9, 5, 7, 8, 9, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1,
    9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11, -1, -1, -1, -1,
    2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7, -1, -1, -1, -1,
    11, 2, 1, 11, 1, 7, 7, 1, 5, -1, -1, -1, -1, -1, -1, -1,
    9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11, -1, -1, -1, -1,
    5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0, -1,
    11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0, -1,
    11, 10, 5, 7, 11, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 3, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, 0, 1, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 8, 3, 1, 9, 8, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1,
    1, 6, 5, 2, 6, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 6, 5, 1, 2, 6, 3, 0, 8, -1, -1, -1, -1, -1, -1, -1,
    9, 6, 5, 9, 0, 6, 0, 2, 6, -1, -1, -1, -1, -1, -1, -1,
    5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, -1, -1, -1, -1,
    2, 3, 11, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    11, 0, 8, 11, 2, 0, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1,
    0, 1, 9, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1,
    5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11, -1, -1, -1, -1,
    6, 3, 11, 6, 5, 3, 5, 1, 3, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6, -1, -1, -1, -1,
    3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, -1, -1, -1, -1,
    6, 5, 9, 6, 9, 11, 11, 9, 8, -1, -1, -1, -1, -1, -1, -1,
    5, 10, 6, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 3, 0, 4, 7, 3, 6, 5, 10, -1, -1, -1, -1, -1, -1, -1,
    1, 9, 0, 5, 10, 6, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1,
    10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, -1, -1, -1, -1,
    6, 1, 2, 6, 5, 1, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, -1, -1, -1, -1,
    8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, -1, -1, -1, -1,
    7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9, -1,
    3, 11, 2, 7, 8, 4, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1,
    5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11, -1, -1, -1, -1,
    0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1,
    9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6, -1,
    8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6, -1, -1, -1, -1,
    5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11, -1,
    0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7, -1,
    6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9, -1, -1, -1, -1,
    10, 4, 9, 6, 4, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 10, 6, 4, 9, 10, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1,
    10, 0, 1, 10, 6, 0, 6, 4, 0, -1, -1, -1, -1, -1, -1, -1,
    8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10, -1, -1, -1, -1,
    1, 4, 9, 1, 2, 4, 2, 6, 4, -1, -1, -1, -1, -1, -1, -1,
    3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, -1, -1, -1, -1,
    0, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    8, 3, 2, 8, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1,
    10, 4, 9, 10, 6, 4, 11, 2, 3, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6, -1, -1, -1, -1,
    3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10, -1, -1, -1, -1,
    6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1, -1,
    9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3, -1, -1, -1, -1,
    8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1, -1,
    3, 11, 6, 3, 6, 0, 0, 6, 4, -1, -1, -1, -1, -1, -1, -1,
    6, 4, 8, 11, 6, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    7, 10, 6, 7, 8, 10, 8, 9, 10, -1, -1, -1, -1, -1, -1, -1,
    0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10, -1, -1, -1, -1,
    10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0, -1, -1, -1, -1,
    10, 6, 7, 10, 7, 1, 1, 7, 3, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, -1, -1, -1, -1,
    2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9, -1,
    7, 8, 0, 7, 0, 6, 6, 0, 2, -1, -1, -1, -1, -1, -1, -1,
    7, 3, 2, 6, 7, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7, -1, -1, -1, -1,
    2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7, -1,
    1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11, -1,
    11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1, -1, -1, -1, -1,
    8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6, -1,
    0, 9, 1, 11, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0, -1, -1, -1, -1,
    7, 11, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    3, 0, 8, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 1, 9, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    8, 1, 9, 8, 3, 1, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1,
    10, 1, 2, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 10, 3, 0, 8, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1,
    2, 9, 0, 2, 10, 9, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1,
    6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8, -1, -1, -1, -1,
    7, 2, 3, 6, 2, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    7, 0, 8, 7, 6, 0, 6, 2, 0, -1, -1, -1, -1, -1, -1, -1,
    2, 7, 6, 2, 3, 7, 0, 1, 9, -1, -1, -1, -1, -1, -1, -1,
    1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, -1, -1, -1, -1,
    10, 7, 6, 10, 1, 7, 1, 3, 7, -1, -1, -1, -1, -1, -1, -1,
    10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8, -1, -1, -1, -1,
    0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7, -1, -1, -1, -1,
    7, 6, 10, 7, 10, 8, 8, 10, 9, -1, -1, -1, -1, -1, -1, -1,
    6, 8, 4, 11, 8, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    3, 6, 11, 3, 0, 6, 0, 4, 6, -1, -1, -1, -1, -1, -1, -1,
    8, 6, 11, 8, 4, 6, 9, 0, 1, -1, -1, -1, -1, -1, -1, -1,
    9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6, -1, -1, -1, -1,
    6, 8, 4, 6, 11, 8, 2, 10, 1, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6, -1, -1, -1, -1,
    4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9, -1, -1, -1, -1,
    10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3, -1,
    8, 2, 3, 8, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1,
    0, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8, -1, -1, -1, -1,
    1, 9, 4, 1, 4, 2, 2, 4, 6, -1, -1, -1, -1, -1, -1, -1,
    8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1, -1, -1, -1, -1,
    10, 1, 0, 10, 0, 6, 6, 0, 4, -1, -1, -1, -1, -1, -1, -1,
    4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3, -1,
    10, 9, 4, 6, 10, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 9, 5, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 3, 4, 9, 5, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1,
    5, 0, 1, 5, 4, 0, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1,
    11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5, -1, -1, -1, -1,
    9, 5, 4, 10, 1, 2, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1,
    6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5, -1, -1, -1, -1,
    7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2, -1, -1, -1, -1,
    3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6, -1,
    7, 2, 3, 7, 6, 2, 5, 4, 9, -1, -1, -1, -1, -1, -1, -1,
    9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7, -1, -1, -1, -1,
    3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0, -1, -1, -1, -1,
    6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8, -1,
    9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7, -1, -1, -1, -1,
    1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4, -1,
    4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10, -1,
    7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10, -1, -1, -1, -1,
    6, 9, 5, 6, 11, 9, 11, 8, 9, -1, -1, -1, -1, -1, -1, -1,
    3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5, -1, -1, -1, -1,
    0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11, -1, -1, -1, -1,
    6, 11, 3, 6, 3, 5, 5, 3, 1, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6, -1, -1, -1, -1,
    0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10, -1,
    11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5, -1,
    6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3, -1, -1, -1, -1,
    5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, -1, -1, -1, -1,
    9, 5, 6, 9, 6, 0, 0, 6, 2, -1, -1, -1, -1, -1, -1, -1,
    1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8, -1,
    1, 5, 6, 2, 1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6, -1,
    10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0, -1, -1, -1, -1,
    0, 3, 8, 5, 6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    10, 5, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    11, 5, 10, 7, 5, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    11, 5, 10, 11, 7, 5, 8, 3, 0, -1, -1, -1, -1, -1, -1, -1,
    5, 11, 7, 5, 10, 11, 1, 9, 0, -1, -1, -1, -1, -1, -1, -1,
    10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1, -1, -1, -1, -1,
    11, 1, 2, 11, 7, 1, 7, 5, 1, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11, -1, -1, -1, -1,
    9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7, -1, -1, -1, -1,
    7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2, -1,
    2, 5, 10, 2, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1,
    8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5, -1, -1, -1, -1,
    9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2, -1, -1, -1, -1,
    9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2, -1,
    1, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 7, 0, 7, 1, 1, 7, 5, -1, -1, -1, -1, -1, -1, -1,
    9, 0, 3, 9, 3, 5, 5, 3, 7, -1, -1, -1, -1, -1, -1, -1,
    9, 8, 7, 5, 9, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    5, 8, 4, 5, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1,
    5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0, -1, -1, -1, -1,
    0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5, -1, -1, -1, -1,
    10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4, -1,
    2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8, -1, -1, -1, -1,
    0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11, -1,
    0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5, -1,
    9, 4, 5, 2, 11, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4, -1, -1, -1, -1,
    5, 10, 2, 5, 2, 4, 4, 2, 0, -1, -1, -1, -1, -1, -1, -1,
    3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9, -1,
    5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2, -1, -1, -1, -1,
    8, 4, 5, 8, 5, 3, 3, 5, 1, -1, -1, -1, -1, -1, -1, -1,
    0, 4, 5, 1, 0, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5, -1, -1, -1, -1,
    9, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 11, 7, 4, 9, 11, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1,
    0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11, -1, -1, -1, -1,
    1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11, -1, -1, -1, -1,
    3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4, -1,
    4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2, -1, -1, -1, -1,
    9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3, -1,
    11, 7, 4, 11, 4, 2, 2, 4, 0, -1, -1, -1, -1, -1, -1, -1,
    11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4, -1, -1, -1, -1,
    2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9, -1, -1, -1, -1,
    9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7, -1,
    3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10, -1,
    1, 10, 2, 8, 7, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 9, 1, 4, 1, 7, 7, 1, 3, -1, -1, -1, -1, -1, -1, -1,
    4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1, -1, -1, -1, -1,
    4, 0, 3, 7, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    4, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    3, 0, 9, 3, 9, 11, 11, 9, 10, -1, -1, -1, -1, -1, -1, -1,
    0, 1, 10, 0, 10, 8, 8, 10, 11, -1, -1, -1, -1, -1, -1, -1,
    3, 1, 10, 11, 3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 2, 11, 1, 11, 9, 9, 11, 8, -1, -1, -1, -1, -1, -1, -1,
    3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9, -1, -1, -1, -1,
    0, 2, 11, 8, 0, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    3, 2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    2, 3, 8, 2, 8, 10, 10, 8, 9, -1, -1, -1, -1, -1, -1, -1,
    9, 10, 2, 0, 9, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8, -1, -1, -1, -1,
    1, 10, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    1, 3, 8, 9, 1, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 9, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 3, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
  )


}
package Russoul.lib.context.scene

import Russoul.lib.common.math.CollisionEngine
import Russoul.lib.common.math.geometry.simple.{AABB, OBB, Ray, Rectangle}
import Russoul.lib.common.math.linear.vec3
import Russoul.lib.common.utils.vector
import Russoul.lib.context.scene.structure.ChunkVoxelizedOld

import scala.language.postfixOps
import scala.util.control.{Breaks, ControlThrowable}

/**
  * Created by Russoul on 19.07.2016.
  */

/**
  *
  * @param center
  * @param size
  * @param parent
  *
  *               TODO redundant
  *
  *               if a node has children it must have chunks deeper into the leaves; if chunk gets deleted but it was the only one in the node, node gets deleted too;
  *               if in the process of deletion of child nodes of the parent node the last one becomes empty it gets deleted and so on and on recursively
  */

//Block Aligned Octree
class BAOctree(private val center:vec3, private val size:Float, private val parent:BAOctree)
{
  private val waiting = new vector[ChunkVoxelizedOld]
  private val chunks = new vector[ChunkVoxelizedOld]
  
  var children = new Array[BAOctree](8)
  var activeNodes:Byte = 0

  def addToQueue(obj:ChunkVoxelizedOld) = {
    waiting += obj

    this
  }
  def addToChunks(obj:ChunkVoxelizedOld) = {
    chunks += obj

    this
  }

  private def genMin():vec3 =
  {
    center - vec3(size/2,size/2,size/2)
  }

  private def genMax():vec3 =
  {
    center + vec3(size/2,size/2,size/2)
  }

  def genBound() = new AABB(center.copy(), vec3(size/2, size/2, size/2))

  def genChildrenBounds():vector[AABB] =
  {
    val AABBs = new vector[AABB]

    val min = genMin()
    val max = genMax()



    AABBs +=  AABB.genFromMinMax(min.copy(), center.copy()) //counter-clockwise, y increasing
    AABBs +=  AABB.genFromMinMax(vec3(min.x, min.y, center.z), vec3(center.x, center.y, max.z))       //io  //oo  //oo  //oi
    AABBs +=  AABB.genFromMinMax(vec3(center.x, min.y, center.z), vec3(max.x, center.y, max.z)) //y   //oo  //io  //oi  //oo
    AABBs +=  AABB.genFromMinMax(vec3(center.x, min.y, min.z), vec3(max.x, center.y, center.z))

    AABBs +=  AABB.genFromMinMax(vec3(min.x, center.y, min.z), vec3(center.x, max.y, center.z))
    AABBs +=  AABB.genFromMinMax(vec3(min.x, center.y, center.z), vec3(center.x, max.y, max.z))
    AABBs +=  AABB.genFromMinMax(vec3(center.x, center.y, center.z), vec3(max.x, max.y, max.z))//y+1
    AABBs +=  AABB.genFromMinMax(vec3(center.x, center.y, min.z), vec3(max.x, max.y, center.z))

    AABBs
  }

  def buildTree(): Unit =
  {
    if(waiting.nonEmpty)
    {

      for (i <- waiting.indices)
      {
        val obj = waiting(i)

        //new extent (former extent/2)
        if(size/2 >= CVoxel.BLOCK_LEAF_MIN_SIZE)
        {

          val aabbs = genChildrenBounds()

          var outside = true
          var intersects = false
          for (j <- 0 until 8 if intersects || outside ) {
            val check:Int = CollisionEngine.checkOBBOBBi(obj.bound, new OBB(aabbs(j)))
            def input(): Unit =
            {
              if(children(j) == null)
              {
                children(j) = new BAOctree(aabbs(j).center, size/2, this)
              }
              children(j).waiting.+=(waiting(i))
              activeNodes = (activeNodes | (1 << i)).toByte
            }

            def inter(): Unit =
            {
              if(children(j) == null)
              {
                children(j) = new BAOctree(aabbs(j).center, size/2, this)
              }
              children(j).chunks.+=(waiting(i))
              activeNodes = (activeNodes | (1 << i)).toByte
            }


            check match{
              case 1 => //intersects
                inter()
                outside = false
                intersects = true
              case 2 => //inside
                input()
                outside = false
                intersects = false
              case 0 => //outside

            }
          }


        }
        else
        {
          chunks += obj
        }

      }

      waiting.clear()


      if(activeNodes != 0)
      {
        for (i <- 0 until 8) {
          if(children(i) != null) children(i).buildTree()
        }
      }
    }
  }

  def genRemoteness(offset:Int = 0):Int =
  {
    if(parent != null){
      parent.genRemoteness(offset + 1)
    }else{
      offset
    }
  }

  def findAllNonEmptyNodes(list:vector[AABB]): Unit =
  {
    if(chunks.nonEmpty) list += new AABB(center, vec3(size/2,size/2,size/2))

    for (i <- 0 until 8 ) {
      val c = children(i)
      if (c != null) c.findAllNonEmptyNodes(list)
    }

  }

  def findAllChunks(list:vector[ChunkVoxelizedOld]): Unit =
  {
    list ++= chunks
    for (i <- 0 until 8 )
    {
      val c = children(i)
      if (c != null) c.findAllChunks(list)
    }
  }

  /**
    *Not recursive !, just children of current remoteness
    */
  def processAllIntersections(space:OBB, @throws(classOf[ControlThrowable]) func:(BAOctree) => Any): Boolean =
  {
    Breaks.tryBreakable
    {
      val all = genChildrenBounds()

      for(i <- all.indices) {
        val b = all(i)
        if(CollisionEngine.checkOBBAABBSeparatingAxisTheorem(space, b) && children(i) != null) {
          func(children(i))
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
    * processes all intersections including child nodes of previously proccessed parent
    * starting from parent ending with leaf nodes
    *
    * rec function inside for proper Breaks
    *
    * @param processed list of already processed chunks
    * @param func chunk collided with, first point of intersection, chunk's rectangle of intersection
    * @return if the process completed fully => true, if broken => false
    */
  def processAllIntersectionsRecursive(ray:Ray, @throws(classOf[ControlThrowable]) func:(ChunkVoxelizedOld,Float,Rectangle)=>Any, processed:vector[ChunkVoxelizedOld]): Boolean =
  {
    Breaks.tryBreakable
    {

      def rec(tree:BAOctree):Unit =
      {
        for(chunk <- tree.chunks){ //self
          val col = CollisionEngine.checkRayBox(ray, chunk.bound)
          if(col.isDefined && !processed.exists(_ eq chunk)){ //DO NOT PROCESS THE SAME CHUNKS
            processed += chunk
            func(chunk, col.get._1, col.get._2)
          }
        }

        for(child <- tree.children if child != null){
          val res = CollisionEngine.checkRayBox(ray, child.genBound())
          if(res.isDefined){ //child
            rec(child)
          }
        }
      }

      rec(this)

      true
    }catchBreak
    {
      false
    }

  }



  def findAllUniqueIntersections(ray:Ray, out:vector[ChunkVoxelizedOld]): Unit =
  {
    def rec(tree:BAOctree):Unit =
    {
      for(chunk <- tree.chunks){ //self
        if(CollisionEngine.checkRayBox(ray, chunk.bound).isDefined && !out.exists(_ eq chunk)){
          out += chunk
        }

      }

      for(child <- tree.children if child != null){
        val res = CollisionEngine.checkRayBox(ray, child.genBound())
        if(res.isDefined){ //child
          rec(child)
        }
      }
    }

    rec(this)

  }


  def findAllUniqueIntersections(obb:OBB, out:vector[ChunkVoxelizedOld]): Unit =
  {
    def rec(tree:BAOctree):Unit =
    {
      for(chunk <- tree.chunks){ //self
        if(CollisionEngine.checkOBBOBB(chunk.bound, obb) && !out.exists(_ eq chunk)){
          out += chunk
        }

      }

      for(child <- tree.children if child != null){
        val b = child.genBound()
        val res = CollisionEngine.checkOBBOBB(new OBB(b.center, vec3(1,0,0), vec3(0,1,0), b.extent.x, b.extent.y, b.extent.z), obb)
        if(res){ //child
          rec(child)
        }
      }
    }

    rec(this)

  }

  def findAllUniqueIntersections(aabb:AABB, out:vector[ChunkVoxelizedOld]): Unit =
  {
    def rec(tree:BAOctree):Unit =
    {
      for(chunk <- tree.chunks){ //self
        if(CollisionEngine.checkOBBAABB(chunk.bound, aabb) != 0 && !out.exists(_ eq chunk)){
          out += chunk
        }

      }

      for(child <- tree.children if child != null){
        val b = child.genBound()
        val res = CollisionEngine.checkOBBAABB(new OBB(b.center, vec3(1,0,0), vec3(0,1,0), b.extent.x, b.extent.y, b.extent.z), aabb)
        if(res != 0){ //child
          rec(child)
        }
      }
    }

    rec(this)

  }

  /**
    *
    * @param func (chunk, Node containing the chunk)
    * includes current chunk
    */
  def processInnerChunks(@throws(classOf[ControlThrowable]) func: (ChunkVoxelizedOld, BAOctree) => Unit): Boolean =
  {
    Breaks.tryBreakable
    {
      def rec(tree:BAOctree, func:(ChunkVoxelizedOld, BAOctree)=> Unit):Unit =
      {
        if(tree.chunks.nonEmpty){
          for(c <- tree.chunks){
            func(c, this)
          }
        }
        if(tree.activeNodes != 0){
          for(child <- tree.children if child != null) child.processInnerChunks(func)
        }
      }

      rec(this, func)

      true
    }catchBreak{
      false
    }
  }


}

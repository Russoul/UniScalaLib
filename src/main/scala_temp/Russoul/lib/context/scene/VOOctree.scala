package Russoul.lib.context.scene

import Russoul.lib.common.math.CollisionEngine
import Russoul.lib.common.math.immutable.geometry.complex.Frustum
import Russoul.lib.common.math.immutable.geometry.simple.{OBB, Plane, Ray, Rectangle}
import Russoul.lib.common.math.immutable.linear.vec3
import Russoul.lib.common.utils.vector
import Russoul.lib.context.scene.structure.Voxel
import Russoul.lib.context.scene.structure.sample.{VoxelData, VoxelSample}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.control.{Breaks, ControlThrowable}

/**
  * Created by Russoul on 18.07.2016.
  */

/**
  *
  * @param bound
  * @param MIN_SIZE
  * @param parent
  *
  *  if a node has children it must have voxels deeper into the leaves; if voxel gets deleted but it was the only one in the node, node gets deleted too;
  *  if in the process of deletion of child nodes of the parent node the last one becomes empty it gets deleted and so on and on recursively
  */


class VOOctree(val bound:OBB,val MIN_SIZE:Float, val parent:VOOctree = null)
{

  var voxelData:VoxelData = null
  var children = new Array[VOOctree](8)
  var activeNodes:Byte = 0

/*
  /**
    *
    * @param v
    * @return index of a voxel in global parent buffer of voxels
    */
  private def addVoxelSynchronized(v:Voxel): Int =
  {
    synchronized
    {
      voxels += v
      voxels.size - 1
    }
  }*/


  def getBound() = bound

  def asVoxel() = new Voxel(bound, voxelData)


  def genChildrenBounds():vector[OBB] =
  {
    val bs = new vector[OBB]

    val center = bound.center
    val min = bound.genMin()
    val max = bound.genMax()

    val l = bound.right ^ bound.up
    val r = bound.right
    val u = bound.up

    val ern =  bound.extentRight/2
    val eun = bound.extentUp/2
    val eln = bound.extentLook/2

    val c1 = min + l*eln + r*ern + u*eun
    val c2 = min + l*(3*eln) + r*ern + u*eun
    val c3 = min + l* eln + r*(3*ern) + u*eun
    val c4 = min + l*(3*eln) + r*(3*ern) + u*eun
    val c5 = min + l* eln + r* ern + u*(3*eun)
    val c6 = min + l*(3*eln) + r* ern + u*(3*eun)
    val c7 = min + l* eln + r*(3*ern) + u*(3*eun)
    val c8 = min + l*(3*eln) + r*(3*ern) + u*(3*eun)

    bs += new OBB(c1, r, u, ern, eun, eln)
    bs += new OBB(c2, r, u, ern, eun, eln)
    bs += new OBB(c3, r, u, ern, eun, eln)
    bs += new OBB(c4, r, u, ern, eun, eln)
    bs += new OBB(c5, r, u, ern, eun, eln)
    bs += new OBB(c6, r, u, ern, eun, eln)
    bs += new OBB(c7, r, u, ern, eun, eln)
    bs += new OBB(c8, r, u, ern, eun, eln)

    bs
  }

  def buildTree(waiting:vector[VoxelSample]): Unit = //concurrent
  {


    if(waiting.nonEmpty)
    {

      val waitings = new Array[vector[VoxelSample]](8)
      for(i <- 0 until 8 ) waitings(i) = new vector[VoxelSample]

      for (i <- waiting.indices )
      {
        val obj = waiting(i)

        //new extent (former extent/2)
        if(bound.extentLook >= MIN_SIZE && bound.extentRight >= MIN_SIZE && bound.extentUp >= MIN_SIZE)
        {

          val obbs = genChildrenBounds()

          var outside = true
          for (j <- 0 until 8 if outside) {
            if(CollisionEngine.checkPointOBB(obj.point, obbs(j)))
            {

              if(children(j) == null)
              {
                children(j) = new VOOctree(obbs(j), MIN_SIZE, this)
              }
              waitings(j) += obj
              activeNodes = (activeNodes | (1 << i)).toByte
              outside = false

              //if current tree is a voxel => make it so it is not !
              voxelData = null
            }
          }

          //if still outside - voxel is outside parent bounding, discard it (automatically)

        }
        else
        {
          voxelData = obj.data.copy()
        }

      }


      //waiting.clear() not needed, just garbage collect objects



      import scala.concurrent.ExecutionContext.Implicits.global
      if(activeNodes != 0)
      {


        if(parent == null){
          val futures = Future.traverse(children.indices.toList)(i => if(children(i) != null)
            Future{children(i).buildTree(waitings(i))} else Future.successful(Unit))

          Await.result(futures, Duration.Inf)


        }else{
          for(i <- children.indices if children(i) != null) children(i).buildTree(waitings(i))
        }


      }

      //if(activeNodes != 0) for(i <- children.indices  if children(i) != null) children(i).buildTree(waitings(i))
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

  def findGlobalParent():VOOctree =
  {
    if(parent != null)
      parent.findGlobalParent()
    else
      this
  }



  /**
    * check of parent node included
    *
    * @param frustum -
    * @param func -
    * @return
    */
  def processFrustumCulling(frustum:Frustum, @throws(classOf[ControlThrowable]) func:(VOOctree)=>Any) : Boolean = {
    Breaks.tryBreakable{

      val n = frustum.genNearPlane()
      val f = frustum.genFarPlane()
      val e = frustum.genExtraPlanes()

      val planes = new vector[Plane]

      planes += new Plane(n.genVertices()(0), -n.genNormal())
      planes += new Plane(f.genVertices()(0), f.genNormal())
      for (i <- 0 until 4 ) planes += e(i)

      def rec(node:VOOctree): Unit ={
        if(node.activeNodes != 0){
          for(child <- node.children if child != null){
            val res = CollisionEngine.checkBoxFrustum(child.bound, planes)
            if(res != 2) rec(child)
          }
        }else{
          if(node.voxelData != null) func(node)
        }
      }

      if(CollisionEngine.checkBoxFrustum(this.bound, planes) != 2){ //check of parent node
        rec(this)
      }

      true
    }catchBreak{
      false
    }
  }


  /**
    * only leaf nodes do count
    * @param ray -
    * @param out -
    */
  def findAllUniqueIntersections(ray:Ray, out:vector[(VOOctree,Float,Rectangle)]):Unit =
  {
    def rec(tree:VOOctree): Unit ={
      for(child <- tree.children if child != null)
      {
        val c = CollisionEngine.checkRayBox(ray, child.getBound())

        if(c.isDefined){
          if(child.voxelData != null){ //voxel
            out += (child,c.get._1,c.get._2)
          }else{
            rec(child)
          }
        }
      }
    }

    for(child <- children if child != null)
    {
      val c = CollisionEngine.checkRayBox(ray, child.getBound())

      if(c.isDefined){
        if(child.voxelData != null){ //voxel
          out += (child,c.get._1,c.get._2)
        }else{
          rec(child)
        }
      }
    }

  }

  def findVoxel(p:vec3):Option[VOOctree] =
  {
    def rec(tree:VOOctree):Option[VOOctree] =
    {
      if(tree.voxelData == null){
        val bounds = tree.genChildrenBounds()
        for(i <- 0 until 8)
        {
          val b = bounds(i)
          if(CollisionEngine.checkPointOBB(p, b)){
            return rec(tree.children(i))
          }
        }
        None
      }else{
        Some(tree)
      }

    }

    rec(this)
  }

  def deleteNode(): Unit =
  {
    val p = this.parent

    if(p != null)
    {
      for(i <- 0 until 8)
      {
        if(p.children(i) == this)
        {
          p.children(i) = null
          p.activeNodes = (p.activeNodes ^ (1 << i)).toByte
          if(p.activeNodes == 0)
          { //parent is out of nodes => delete it
            p.deleteNode()
          }
        }
      }
    }else{
      for(i <- 0 until 8) children(i) = null
    }
  }

  def findNearestNode(in:vector[(VOOctree, Float, Rectangle)]): (VOOctree, Float, Rectangle) =
  {
    var tmin:Float = 0
    var imin = -1
    var first = true

    for(i <- 0 until in.size ){
      val got = in(i)

      if(first){
        tmin = got._2
        imin = i
        first = false
      }else if(got._2 < tmin){
        tmin = got._2
        imin = i
      }
    }

    in(imin)
  }

  def findAllUniqueIntersections(obb: OBB, out:vector[VOOctree]):Unit =
  {
    def rec(tree:VOOctree): Unit ={
      for(child <- tree.children if child != null)
      {
        if(CollisionEngine.checkOBBOBBSeparatingAxisTheorem(obb, child.getBound())){
          if(child.voxelData != null){ //voxel
            out += child
          }else{
            rec(child)
          }
        }
      }
    }

    for(child <- children if child != null)
    {
      if(CollisionEngine.checkOBBOBBSeparatingAxisTheorem(obb, child.getBound())){
        if(child.voxelData != null){ //voxel
          out += child
        }else{
          rec(child)
        }
      }
    }

  }


  /**
    *
    * @param ray -
    * @param func -
    * @return processes all voxel node intersections
    */
  def processAllIntersectionsRecursive(ray:Ray, @throws(classOf[ControlThrowable]) func:(VOOctree,Float,Rectangle)=>Any):Boolean =
  {
    Breaks.tryBreakable
    {

      def rec(tree:VOOctree, res:Option[(Float,Rectangle)]) : Unit =
      {
        if(tree.activeNodes != 0){
          for(child <- tree.children if child != null){
            val res = CollisionEngine.checkRayBox(ray, child.bound)
            rec(child, res)
          }
        }else{ //activeNodes == 0 => its voxel or empty chunk
          if(res.isDefined && tree.voxelData != null){
            func(tree,res.get._1, res.get._2)
          }
        }
      }

      rec(this, CollisionEngine.checkRayBox(ray, this.bound))

      true
    }catchBreak
    {
      false
    }
  }

  def findAllVoxels(list:vector[Voxel]): Unit =
  {
    def rec(node:VOOctree): Unit =
    {
      if(node.activeNodes != 0){
        for(child <- node.children if child != null) rec(child)
      }else{
        list += node.asVoxel()
      }
    }

    rec(this)

  }

  /**
    *
    * @param box -
    * @param func -
    * @return processes all voxel node intersections
    */
  def processAllIntersectionsRecursive(box:OBB, @throws(classOf[ControlThrowable]) func:(VOOctree)=>Any):Boolean =
  {
    Breaks.tryBreakable
    {

      def rec(tree:VOOctree, inter:Boolean) : Unit =
      {
        if(tree.activeNodes != 0){
          for(child <- tree.children if child != null){
            val res = CollisionEngine.checkOBBOBBSeparatingAxisTheorem(box, child.bound)
            rec(child, res)
          }
        }else{
          if(inter && tree.voxelData != null){
            func(tree)
          }
        }
      }

      rec(this, CollisionEngine.checkOBBOBBSeparatingAxisTheorem(box, this.bound))

      true
    }catchBreak {
      false
    }
  }
}

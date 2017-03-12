package Russoul.lib.common.math

import Russoul.lib.common.math.immutable.geometry.simple._
import Russoul.lib.common.math.immutable.linear.vec3
import Russoul.lib.common.utils.vector

import scala.language.postfixOps


object CollisionEngine
{
  /**
    *
    * @param ray
    * @return unit distance from start of the ray to the point of intersection
    */
  def checkRayRectangle(ray:Ray, rec: Rectangle):Option[Float] = {

    val n = rec.genNormal()

    val denom = ray.dir * n


    if(math.abs(denom) < 1e-6){
      None
    }else{
      val temp = (rec.center - rec.up - rec.right) - ray.start
      val num = temp * n
      val t = num/denom
      if(t >= 0){ //in front the ray
        val point = ray.start + ray.dir * t
        val centerPoint = point - rec.center
        val projRight = centerPoint * rec.right.normalize()
        val projUp = centerPoint * rec.up.normalize()


        val rightMag = rec.right.squareLength()
        val upMag = rec.up.squareLength()

        if(projRight*projRight <= rightMag && projUp*projUp <= upMag) {
          Some(t)
        }
        else
          None
      }else{
        None
      }
    }


  }


  private def checkRayBox(ray:Ray, recs:vector[Rectangle]):Option[(Float, Rectangle)] =
  {
    var tmin = -1F
    var re:Rectangle = null

    for(rec <- recs){
      val res = checkRayRectangle(ray, rec)
      res match{
        case None =>
        case e:Some[Float] =>
          if(re == null)
          {
            tmin = e.get
            re = rec
          }
          else{
            if(e.get < tmin){
              tmin = e.get
              re = rec
            }
          }
      }
    }

    if(re != null) Option((tmin, re)) else None
  }

  /**
    *
    * @param ray
    * @param obb
    * @return unit dist, face of intersection
    */
  def checkRayBox(ray:Ray, obb:OBB):Option[(Float, Rectangle)] ={
    val recs = obb.genRectangles()

    checkRayBox(ray, recs)
  }

  /**
    *
    * @param ray
    * @param aabb
    * @return unit dist, face of intersection
    */
  def checkRayBox(ray:Ray, aabb:AABB):Option[(Float, Rectangle)] ={
    val recs = aabb.genRectangles()

    checkRayBox(ray, recs)
  }



  def checkAABBAABB(checkThis:AABB, checkWith:AABB): Boolean =
  {
    val amin = checkThis.genMin()
    val amax = checkThis.genMax()

    val bmin = checkWith.genMin()
    val bmax = checkWith.genMax()


     amin.x >= bmin.x && amax.x <= bmax.x &&
            amin.y >= bmin.y && amax.y <= bmax.y &&
            amin.z >= bmin.z && amax.z <= bmax.z
  }

  def checkOBBOBBSeparatingAxisTheorem(checkThis:OBB, checkWith:OBB):Boolean =
  {

    val normals = new vector[vec3]

    val recs = checkThis.genRectangles()
    recs ++= checkWith.genRectangles()

    for(i <- 0 until recs.size/2 )
    {
      normals += recs(2*i).genNormal(); //getting all 6 unique normals, 3 from each OBB
    }

    for(i <- 0 until 3 )
    {
      normals += (normals(i) ^ normals(3)) //getting all 9 unique normals from crosses of edges 3X3 of each OBB
      normals += (normals(i) ^ normals(4))
      normals += (normals(i) ^ normals(5))
    }
    //15 normals total (those are separation axes)

    val cToC = checkWith.center - checkThis.center

    val look1 = checkThis.right^checkThis.up
    val look2 = checkWith.right^checkWith.up


    for(i <- 0 until normals.size )
    {
      val n = normals(i)

      val S = math.abs(cToC * n)

      val s1 = math.abs(( checkThis.right * checkThis.extentRight ) * n) + math.abs( (checkThis.up * checkThis.extentUp) * n) + math.abs ( (look1 * checkThis.extentLook) * n)
      val s2 = math.abs(( checkWith.right * checkWith.extentRight ) * n) + math.abs( (checkWith.up * checkWith.extentUp) * n) + math.abs ( (look2 * checkWith.extentLook) * n)

      if(S > s1 + s2) return false
    }

    true
  }

  def checkOBBAABBSeparatingAxisTheorem(checkThis:OBB, checkWith:AABB):Boolean =
  {

    val normals = vector[vec3](16)

    val recs = checkThis.genRectangles()
    recs ++= checkWith.genRectangles()

    for(i <- 0 until recs.size/2 )
    {
      normals += recs(2*i).genNormal(); //getting all 6 unique normals, 3 from each OBB
    }

    for(i <- 0 until 3 )
    {
      normals += (normals(i) ^ normals(3)) //getting all 9 unique normals from crosses of edges 3X3 of each OBB
      normals += (normals(i) ^ normals(4))
      normals += (normals(i) ^ normals(5))
    }
    //15 normals total (those are separation axes)

    val cToC = checkWith.center - checkThis.center

    val look1 = checkThis.right^checkThis.up


    val look2 = vec3(0,0,1)
    val r2 = vec3(1,0,0)
    val u2 = vec3(0,1,0)


    for(i <- normals.indices )
    {
      val n = normals(i)

      val S = math.abs(cToC * n)

      val s1 = math.abs(( checkThis.right * checkThis.extentRight ) * n) + math.abs( (checkThis.up * checkThis.extentUp) * n) + math.abs ( (look1 * checkThis.extentLook) * n)
      val s2 = math.abs(( r2 * checkWith.extent.x ) * n) + math.abs( (u2 * checkWith.extent.y) * n) + math.abs ( (look2 * checkWith.extent.z) * n)

      if(S > s1 + s2) return false
    }

    true
  }

  def checkPointSphere(point:vec3, sphere:Sphere):Boolean =
  {
    (point-sphere.center).squareLength() <= sphere.rad*sphere.rad
  }


  def checkPointOBB(p:vec3, b:OBB):Boolean =
  {
    val r = b.right
    val u = b.up
    val l = r ^ u

    val er = b.extentRight
    val eu = b.extentUp
    val el = b.extentLook

    val d = p - b.center

    val v1 = d * r
    val v2 = d * u
    val v3 = d * l

    v1 >= -er && v1 <= er && v2 >= -eu && v2 <= eu && v3 >= -el && v3 <= el

  }

  def checkPointOBBi(p:vec3, b:OBB):Int =
  {
    val r = b.right
    val u = b.up
    val l = r ^ u

    val er = b.extentRight
    val eu = b.extentUp
    val el = b.extentLook

    val d = p - b.center

    val v1 = d * r
    val v2 = d * u
    val v3 = d * l

    if(v1 >= -er && v1 <= er && v2 >= -eu && v2 <= eu && v3 >= -el && v3 <= el) 1 else 0

  }


  def checkPointAABB(point: vec3, container: AABB) =
  {
    val cmin = container.genMin()
    val cmax = container.genMax()



    cmin.x <= point.x && cmin.y <= point.y && cmin.z <= point.z &&
      cmax.x >= point.x && cmax.y >= point.y && cmax.z >= point.z
  }

  def checkPointAABBi(p:vec3, b:AABB):Int =
  {

    val min = b.genMin()
    val max = b.genMax()

    if(p.x < max.x && p.x > min.x && p.y < max.y && p.y > min.y && p.z < max.z && p.z > min.z) 1 else 0

  }


  /**
    *
    * @param checkThis
    * @param checkWith
    * @return intersection/inside
    */
  def checkOBBOBB(checkThis:OBB, checkWith:OBB): Boolean=
  {
    val points = checkThis.genVertices()

    for(i <- points.indices )
    {
      if(checkPointOBB(points(i), checkWith)) return true //intersection
    }

    //no intersections: A is outside B or A fully contains B


    val fe = (checkWith.right ^ checkWith.up)*checkWith.extentLook
    val ue =  checkWith.up*checkWith.extentUp
    val re = checkWith.right * checkWith.extentRight

    val point =  checkWith.center + ue + re + fe //vertex of B

    checkPointOBB(point, checkThis) //is inside A ? inside : outside

  }







  /**
    * check of vertices
    *
    * 0 - outside
    * 1 - intersects
    * 2 - inside
    */
  def checkOBBAABB(checkThis:OBB, checkWith:AABB) : Int =
  {
    val points = checkThis.genVertices()

    var in:Int = 0

    for(i <- 0 until points.size )
    {
      in += checkPointAABBi(points(i), checkWith)
    }

    in match
    {
      case 0 =>
        val point =  checkWith.genMin() //vertex of B
        if(checkPointOBB(point, checkThis)) 1 else 0 //is inside(intersects) A ? inside : outside
      case 8 => 2
      case _ => 1
    }
  }


  /**
    * check of vertices
    *
    * 0 - outside
    * 1 - intersects
    * 2 - inside
    */
  def checkOBBOBBi(checkThis:OBB, checkWith:OBB) : Int =
  {
    val points = checkThis.genVertices()

    var in:Int = 0

    for(i <- points.indices )
    {
      in += checkPointOBBi(points(i), checkWith)
    }

    in match
    {
      case 0 =>
        val fe = (checkWith.right ^ checkWith.up)*checkWith.extentLook
        val ue =  checkWith.up*checkWith.extentUp
        val re = checkWith.right * checkWith.extentRight

        val point =  checkWith.center + ue + re + fe //vertex of B

        if(checkPointOBB(point, checkThis)) 1 else 0 //is inside(intersects) A ? inside : outside
      case 8 => 2
      case _ => 1
    }
  }

  def checkPointRectangle(p: vec3, rec: Rectangle): Boolean =
  {
    val vs = rec.genVertices()

    val A = vs(0)
    val B = vs(1)
    val C = vs(2)

    val l = p - A

    val AB = B - A
    val AC = C - A

    val lAB = l * AB
    val lAC = l * AC

    0 <= lAB && lAB <= AB * AB && 0 <= lAC && lAC <= AC * AC
  }

  /**
    *
    * @return if point is to the side of plane's normal
    */
  def checkPointPlaneNormal(p: vec3, plane:Plane): Boolean =
  {
    //plane equation: "n*(p-p0) = 0"
    val n = plane.normal
    val d = p - plane.point

    val v = d * n

    v <= 0
  }

  /**
    *
    * @param box
    * @return 0 - inside, 1 - intersects, 2 - outside
    */
  def checkBoxFrustum(box: AABB, planes: vector[Plane]): Int =
  {

    val vertices = box.genVertices()

    checkBoxFrustum(vertices, planes)
  }



  /**
    *
    * @param box
    * @return 0 - inside, 1 - intersects, 2 - outside
    */
  def checkBoxFrustum(box: OBB, planes: vector[Plane]): Int =
  {

    val vertices = box.genVertices()

    checkBoxFrustum(vertices, planes)
  }

  /**
    *
    * @param vertices
    * @return 0 - inside, 1 - intersects, 2 - outside
    */
  private def checkBoxFrustum(vertices: vector[vec3], planes: vector[Plane]): Int =
  {

    var totalIn = 0

    for (i <- 0 until 6 ) {
      //six planes for every frustum
      val plane = planes(i)
      val n = plane.normal

      var inCount = 8
      var pIn = 1

      for (j <- vertices.indices ) {
        val v = vertices(j)
        val toVertex = (v - plane.point).normalize()
        val res = n * toVertex
        if (res < 0) {
          inCount -= 1
          pIn = 0
        }
      }

      if (inCount == 0) return 2
      totalIn += pIn
    }

    if (totalIn == 6) return 0

    1
  }

  /**
    *
    * @param sp1
    * @param sp2
    * @return intersects or inside
    */

  def checkSphereSphere(sp1: Sphere, sp2: Sphere): Boolean =
  {
    val sqdist = (sp1.center - sp2.center).squareLength()
    val sqradDist = (sp1.rad + sp2.rad) * (sp1.rad + sp2.rad)

    sqdist < sqradDist

  }

  /*def checkAABBAndFrustumSlow2(box: AABB, planes: vector[Plane]): Int =
  {

    var result = 0
    var out = 0
    var in = 0

    val vertices = box.genVertices()

    for (i <- 0 until 6 ) {
      in = 0
      out = 0

      for (k <- 0 until 8  if in == 0 || out == 0) {
        val v = vertices(k)
        val toVertex = (v - planes(i).point).normalize()
        val res = planes(i).normal * toVertex
        if (res < 0) {
          out += 1
        } else
          in += 1
      }

      if (in == 0) result = 2
      else if (out != 0) result = 1
    }

    result
  }*/






}

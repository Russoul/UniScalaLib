/*
package russoul.lib.common.math

import java.lang

import russoul.lib.common._
import russoul.lib.common.math.geometry.simple._
import russoul.lib.common.utils.Arr
import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra.Mat

import scala.language.postfixOps
import scala.reflect.ClassTag
import Implicits._
import russoul.lib.common.Ops.VectorSpaceOps
import spire.algebra._
import spire.math._
import spire.implicits._
import singleton.ops._

object CollisionEngine
{

  /*def genGramOrtho3[@tbsp F]()(implicit ev: Field[F], tag : ClassTag[F]): Mat[F] =
  {
    Mat[F](3,3, Array[F](ev.one, ev.zero, ev.zero,
                    ev.zero, ev.one, ev.zero,
                    ev.zero, ev.zero, ev.one))
  }*/

  /**
    *
    * @param ray
    * @return unit distance from start of the ray to the point of intersection
    *
    */
  def checkRayRectangle(ray: Ray, rec: Rectangle): Option[Double] = {

    import Implicits._

    val n = rec.genNormal()

    val denom = ray.dir ⋅ n

    //val debug = new VectorSpaceOps(Vec3[Float](1,1,1)).*(2F)

    if(abs(denom) < 1e-6){
      None
    }else{
      val temp = (rec.center - rec.up - rec.right) - ray.start
      val num = temp dot n
      val t = num/denom
      if(t >= 0){ //in front the ray
        val point = ray.start + ray.dir * t
        val centerPoint = point - rec.center
        val projRight = centerPoint ⋅ rec.right.normalize
        val projUp = centerPoint ⋅ rec.up.normalize


        val rightMag = rec.right.squaredLength()
        val upMag = rec.up.squaredLength()

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


  private def checkRayBox(ray:Ray, recs:Array[Rectangle]):Option[(Double, Rectangle)] =
  {
    var tmin = -1D
    var re:Rectangle = null


    for(rec <- recs){
      val res:Option[Double] = checkRayRectangle(ray, rec)
      res match{
        case None =>
        case e:Some[Double] =>
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
  def checkRayBox(ray:Ray, obb:OBB):Option[(Double, Rectangle)] ={
    val recs:Array[Rectangle] = obb.genRectangles()

    checkRayBox(ray, recs)
  }

  /**
    *
    * @param ray
    * @param aabb
    * @return unit dist, face of intersection
    */
  def checkRayBox(ray:Ray, aabb:AABB):Option[(Double, Rectangle)] ={
    val recs:Array[Rectangle] = aabb.genRectangles()

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

    val normals = new Arr[Double3]

    var recs = checkThis.genRectangles()
    recs = recs ++ checkWith.genRectangles() //TODO fast enough ?

    for(i <- 0 until recs.size/2 )
    {
      normals += recs(2*i).genNormal(); //getting all 6 unique normals, 3 from each OBB
    }

    for(i <- 0 until 3 )
    {
      normals += (normals(i) ⨯ normals(3)) //getting all 9 unique normals from crosses of edges 3X3 of each OBB
      normals += (normals(i) ⨯ normals(4))
      normals += (normals(i) ⨯ normals(5))
    }
    //15 normals total (those are separation axes)

    val cToC = checkWith.center - checkThis.center

    val look1 = checkThis.right ⨯ checkThis.up
    val look2 = checkWith.right ⨯ checkWith.up


    for(i <- 0 until normals.size )
    {
      val n = normals(i)

      val S = abs(cToC ⋅ n)

      val s1 = abs(( checkThis.right * checkThis.extentRight ) ⋅ n) + abs( (checkThis.up * checkThis.extentUp) ⋅ n) + abs ( (look1 * checkThis.extentLook) ⋅ n)
      val s2 = abs(( checkWith.right * checkWith.extentRight ) ⋅ n) + abs( (checkWith.up * checkWith.extentUp) ⋅ n) + abs ( (look2 * checkWith.extentLook) ⋅ n)

      if(S > s1 + s2) return false
    }

    true
  }

  def checkOBBAABBSeparatingAxisTheorem(checkThis:OBB, checkWith:AABB):Boolean =
  {

    val normals = new Arr[Double3](16)

    var recs = checkThis.genRectangles()
    recs = recs ++ checkWith.genRectangles()

    for(i <- 0 until recs.size/2 )
    {
      normals += recs(2*i).genNormal(); //getting all 6 unique normals, 3 from each OBB
    }

    for(i <- 0 until 3 )
    {
      normals += (normals(i) ⨯ normals(3)) //getting all 9 unique normals from crosses of edges 3X3 of each OBB
      normals += (normals(i) ⨯ normals(4))
      normals += (normals(i) ⨯ normals(5))
    }
    //15 normals total (those are separation axes)

    val cToC = checkWith.center - checkThis.center

    val look1 = checkThis.right ⨯ checkThis.up


    val look2 = Vec3(0D,0D,1D)
    val r2 = Vec3(1D,0D,0)
    val u2 = Vec3(0D,1D,0D)


    for(i <- normals.indices )
    {
      val n = normals(i)

      val S = abs(cToC ⋅ n)

      val s1 = abs(( checkThis.right * checkThis.extentRight ) ⋅ n) + abs( (checkThis.up * checkThis.extentUp) ⋅ n) + abs ( (look1 * checkThis.extentLook) ⋅ n)
      val s2 = abs(( r2 * checkWith.extent.x ) ⋅ n) + abs( (u2 * checkWith.extent.y) ⋅ n) + abs ( (look2 * checkWith.extent.z) ⋅ n)

      if(S > s1 + s2) return false
    }

    true
  }

  def checkPointSphere(point:Double3, sphere:Sphere):Boolean =
  {
    (point-sphere.center).squaredLength() <= sphere.rad*sphere.rad
  }


  def checkPointOBB(p:Double3, b:OBB):Boolean =
  {
    val r = b.right
    val u = b.up
    val l = r ⨯ u

    val er = b.extentRight
    val eu = b.extentUp
    val el = b.extentLook

    val d = p - b.center

    val v1 = d ⋅ r
    val v2 = d ⋅ u
    val v3 = d ⋅ l

    v1 >= -er && v1 <= er && v2 >= -eu && v2 <= eu && v3 >= -el && v3 <= el

  }

  def checkPointOBBi(p:Double3, b:OBB):Int =
  {
    val r = b.right
    val u = b.up
    val l = r ⨯ u

    val er = b.extentRight
    val eu = b.extentUp
    val el = b.extentLook

    val d = p - b.center

    val v1 = d ⋅ r
    val v2 = d ⋅ u
    val v3 = d ⋅ l

    if(v1 >= -er && v1 <= er && v2 >= -eu && v2 <= eu && v3 >= -el && v3 <= el) 1 else 0

  }


  def checkPointAABB(point: Double3, container: AABB): Boolean =
  {
    val cmin = container.genMin()
    val cmax = container.genMax()



    cmin.x <= point.x && cmin.y <= point.y && cmin.z <= point.z &&
      cmax.x >= point.x && cmax.y >= point.y && cmax.z >= point.z
  }

  def checkPointAABBi(p:Double3, b:AABB):Int =
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


    val fe = (checkWith.right ⨯ checkWith.up) * checkWith.extentLook
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
  def checkOBBAABBi(checkThis:OBB, checkWith:AABB) : Int =
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
        val fe = (checkWith.right ⨯ checkWith.up)*checkWith.extentLook
        val ue =  checkWith.up*checkWith.extentUp
        val re = checkWith.right * checkWith.extentRight

        val point =  checkWith.center + ue + re + fe //vertex of B

        if(checkPointOBB(point, checkThis)) 1 else 0 //is inside(intersects) A ? inside : outside
      case 8 => 2
      case _ => 1
    }
  }


  //TODO CHECK IF IT DoubleLY WORKS ???
  def checkPointRectangle(p: Double3, rec: Rectangle): Boolean =
  {
    val vs = rec.genVertices()

    val A = vs(0)
    val B = vs(1)
    val C = vs(2)

    val l = p - A

    val AB = B - A
    val AC = C - A

    val lAB = l ⋅ AB
    val lAC = l ⋅ AC

    0 <= lAB && lAB <= (AB ⋅ AB) && 0 <= lAC && lAC <= (AC ⋅ AC)
  }


  /**
    *
    * @return if point is to the side of plane's normal
    */
  def checkPointPlaneNormal(p: Double3, plane:Plane): Boolean =
  {
    //plane equation: "n*(p-p0) = 0"
    val n = plane.normal
    val d = p - plane.point

    val v = d ⋅ n

    v <= 0
  }

  /**
    *
    * @param box
    * @return 0 - inside, 1 - intersects, 2 - outside
    */
  def checkBoxFrustum(box: AABB, planes: Arr[Plane]): Int =
  {

    val vertices = box.genVertices()

    checkBoxFrustum(vertices, planes)
  }



  /**
    *
    * @param box
    * @return 0 - inside, 1 - intersects, 2 - outside
    */
  def checkBoxFrustum(box: OBB, planes: Arr[Plane]): Int =
  {

    val vertices = box.genVertices()

    checkBoxFrustum(vertices, planes)
  }

  /**
    *
    * @param vertices
    * @return 0 - inside, 1 - intersects, 2 - outside
    */
  private def checkBoxFrustum(vertices: Array[Double3], planes: Arr[Plane]): Int =
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
        val toVertex = (v - plane.point).normalize
        val res = n ⋅ toVertex
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
    val sqdist = (sp1.center - sp2.center).squaredLength()
    val sqradDist = (sp1.rad + sp2.rad) * (sp1.rad + sp2.rad)

    sqdist < sqradDist

  }

























  //2D--------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------

  def checkPoint2Rectangle2(p:Double2, rec:Rectangle2):Boolean =
  {
    p.x >= rec.center.x - rec.extent.x && p.x <= rec.center.x + rec.extent.x &&
    p.y >= rec.center.y - rec.extent.y && p.y <= rec.center.y + rec.extent.y
  }


  def checkRectangle2Rectangle2(a:Rectangle2, b:Rectangle2): Boolean =
  {
    a.center.x-a.extent.x <= b.center.x+b.extent.x && a.center.x+a.extent.x >= b.center.x-b.extent.x &&
      a.center.y-a.extent.y <= b.center.y+b.extent.y && a.center.y+a.extent.y >= b.center.y-b.extent.y

  }

  def checkPoint2OBB2(p: Double2, obb: OBB2) : Boolean = {
    val centerPoint = p - obb.center

    val projRight = centerPoint dot obb.right
    if(projRight <= obb.extentRight){
      val projUp = centerPoint dot obb.up
      if(projUp <= obb.extentUp){
        true
      }else{
        false
      }
    }else{
      false
    }
  }


  def checkCircleCircle(a:Circle, b:Circle): Boolean =
  {

    (b.center⋅a.center) <= (a.rad + b.rad) * (a.rad + b.rad)
  }

  def checkCircleRectangle2(a:Circle, b:Rectangle2) : Boolean =
  {
    !(a.center.x + a.rad < b.center.x - b.extent.x || a.center.x - a.rad > b.center.x + b.extent.x ||
      a.center.y + a.rad < b.center.y - b.extent.y || a.center.y - a.rad > b.center.y + b.extent.y)
  }



  /*def penetrationCircleRectangle2(a:Circle, b:Rectangle2) :Option[PenetrationData2] =
  {
    if(!checkCircleRectangle2(a, b)) None
    else{

    }
  }*/


  def checkPoint2Square2(p:Double2, centerOfSquare:Double2, extentOfSquare:Double): Boolean ={
    p.x >= centerOfSquare.x-extentOfSquare &&
      p.x <= centerOfSquare.x + extentOfSquare &&
      p.y >= centerOfSquare.y - extentOfSquare &&
      p.y <= centerOfSquare.y + extentOfSquare
  }



  def checkRectangle2Square2(a:Rectangle2, b:Square2): Boolean =
  {
    a.center.x-a.extent.x <= b.center.x+b.extent && a.center.x+a.extent.x >= b.center.x-b.extent &&
      a.center.y-a.extent.y <= b.center.y+b.extent && a.center.y+a.extent.y >= b.center.y-b.extent
  }

  /**
    *
    * @param a
    * @param b
    * @return singular intersection point, None in other cases
    */
  def checkLine2Line2(a:Line2, b:Line2) : Option[Double2] =
  {


    val A1 = a.end.y - a.start.y
    val B1 = a.start.x - a.end.x
    val C1 = A1*a.start.x + B1*a.start.y

    val A2 = b.end.y - b.start.y
    val B2 = b.start.x - b.end.x
    val C2 = A2*b.start.x + B2*b.start.y

    val det = A1*B2 - B1*A2

    if(det == 0) return None //lines are parallel or coincide we are returning None either way

    val x = (C1*B2 - B1*C2)/det
    val y = (A1*C2 - C1*A2)/det

    val intersection = Vec2(x,y)


    val dif1 = a.end - a.start
    val dif2 = b.end - b.start
    val len1 = dif1.squaredLength()
    val len2 = dif2.squaredLength()
    val difA = intersection - a.start
    val difB = intersection - b.start


    val test1 = dif1 ⋅ difA
    val test2 = dif2 ⋅ difB

    if( test1 > 0 && test1/len1 <= 1 && test2 > 0 && test2/len2 <= 1) Some(intersection) else None

  }

  /**
    *
    * @return point and line of intersection of rectangle
    */
  def checkLine2Rectangle2Min(line:Line2, rec:Rectangle2): Option[(Double2, Line2)] =
  {

    val vertices = rec.genVertices()

    val l0 = Line2Over(vertices(0), vertices(1))
    val l1 = Line2Over(vertices(1), vertices(2))
    val l2 = Line2Over(vertices(2), vertices(3))
    val l3 = Line2Over(vertices(3), vertices(0))

    val i0 = checkLine2Line2(line, l0)
    val i1 = checkLine2Line2(line, l1)
    val i2 = checkLine2Line2(line, l2)
    val i3 = checkLine2Line2(line, l3)

    var min = Double.MaxValue
    var minV = Double2(0D,0D)
    var minL:Line2 = null

    var temp:Double = 0D

    if(i0.isDefined && {temp = i0.get.squaredLength();temp} < min){
      minV = i0.get
      min = temp
      minL = l0
    }
    if(i1.isDefined && {temp = i1.get.squaredLength();temp} < min){
      minV = i1.get
      min = temp
      minL = l1
    }
    if(i2.isDefined && {temp = i2.get.squaredLength();temp} < min){
      minV = i2.get
      min = temp
      minL = l2
    }
    if(i3.isDefined && {temp = i3.get.squaredLength();temp} < min){
      minV = i3.get
      min = temp
      minL = l3
    }

    if(minL != null) Some(minV, minL) else None


  }

  def checkRay2Ray2(a:Ray2, b:Ray2):Option[Double2] =
  {
    val A1 = a.dir.y
    val B1 = -a.dir.x
    val C1 = A1*a.start.x + B1*a.start.y

    val A2 = b.dir.y
    val B2 = -b.dir.x
    val C2 = A2*b.start.x + B2*b.start.y

    val det = A1*B2 - B1*A2

    if(det == 0) return None //lines are parallel or coincide we are returning None either way

    val x = (C1*B2 - B1*C2)/det
    val y = (A1*C2 - C1*A2)/det

    Some(Vec2(x,y))
  }

  def distancePoint2Point2(a:Double2, b:Double2): Double =
  {
    (b-a).length()
  }

  /**
    *
    * @param point
    * @param line
    * @return distance to infinite! line
    */
  def distancePoint2Line2(point: Double2, line: Line2D) : Double = {
    val d = line.start - line.end
    val n = d.⟂
    val vec = point - line.start
    lang.Math.abs((n dot vec) / n.length())
  }


  /**
    *
    * @param circle
    * @param line
    * @return distance and point of closest distance
    */
  def distanceCircleLine(circle:Circle, line:Line2) : (Double,Double2) =
  {

    val v1 = line.end-line.start

    val ray1 = Ray2Over(line.start, v1)
    val ray2 = Ray2Over(circle.center, ray1.dir.⟂())


    val dist = checkRay2Ray2(ray1, ray2).get //can be always found as rays are orthogonal

    val v2 = dist - line.start
    val v3 = line.end - dist

    println(ray2.dir)
    println(dist)

    if((v1 ⋅ v2) * (v1⋅v3) > 0)
      ((circle.center-dist).length()-circle.rad, dist)
    else{
      val l1 = (circle.center - line.start).squaredLength()
      val l2 = (circle.center - line.end).squaredLength()

      val ev = implicitly[Euclidean[Double]]

      if(l1 > l2)
        (ev.sqrt(l2) - circle.rad, line.end)
      else
        (ev.sqrt(l1) - circle.rad, line.start)
    }
  }


  /**
    *
    * @param a
    * @param b
    * @return 8 different situations not including intersection
    *         if intersection ret 0 else 4 dists between corners or 4 dists between edges possible (just draw it to understand)
    *         returning distance
    */
  def distanceRectangle2Square2(a:Rectangle2, b:Square2):Double =
  {


    if(checkRectangle2Square2(a, b)) 0D
    else{
      if(b.center.x - b.extent >= a.center.x + a.extent.x){
        if(b.center.y + b.extent <= a.center.y - a.extent.y){
          distancePoint2Point2(Vec2(a.center.x+a.extent.x, a.center.y - a.extent.y), Vec2(b.center.x - b.extent, b.center.y + b.extent))
        }else if(b.center.y - b.extent >= a.center.y + a.extent.y){
          distancePoint2Point2(Vec2(a.center.x+a.extent.x, a.center.y + a.extent.y), Vec2(b.center.x - b.extent, b.center.y - b.extent))
        }else{
          (b.center.x - b.extent) - (a.center.x + a.extent.x)
        }
      }else if(b.center.x + b.extent <= a.center.x - a.extent.x){
        if(b.center.y - b.extent >= a.center.y + a.extent.y){
          distancePoint2Point2(Vec2(a.center.x-a.extent.x, a.center.y + a.extent.y), Vec2(b.center.x + b.extent, b.center.y - b.extent))
        }else if(b.center.y + b.extent <= a.center.y - a.extent.y){
          distancePoint2Point2(Vec2(a.center.x-a.extent.x, a.center.y - a.extent.y), Vec2(b.center.x + b.extent, b.center.y + b.extent))
        }else{
          (a.center.x - a.extent.x) - (b.center.x + b.extent)
        }
      }else{
        if(b.center.y - b.extent >= a.center.y + a.extent.y){
          (b.center.y - b.extent) - (a.center.y + a.extent.y)
        }else{
          (a.center.y - a.extent.y) - (b.center.y + b.extent)
        }
      }
    }
  }


  //--------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------


  /*def normalOfTouchRectangle2Square2(a:Rectangle2, b:Square2):Option[vec2] =
  {
    if(a.center.x-a.extent.x == b.center.x+b.extent && b.center.y - b.extent < a.center.y + a.extent.y && b.center.y + b.extent > a.center.y - a.extent.y) Some(vec2(-1,0))
    else if(a.center.x+a.extent.x == b.center.x-b.extent  && b.center.y - b.extent < a.center.y + a.extent.y && b.center.y + b.extent > a.center.y - a.extent.y) Some(vec2(1,0))
    else if(a.center.y+a.extent.y == b.center.y-b.extent  && b.center.x - b.extent < a.center.x + a.extent.x && b.center.x + b.extent > a.center.x - a.extent.x) Some(vec2(0,1))
    else if(a.center.y-a.extent.y == b.center.y+b.extent  && b.center.x - b.extent < a.center.x + a.extent.x && b.center.x + b.extent > a.center.x - a.extent.x) Some(vec2(0,-1))
    else None
  }*/


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
        val toVertex = (v - planes(i).point).normalize
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
  }


  def penetrationCircleCircle(a:Circle, b:Circle):Option[PenetrationData2] =
  {
    if(!checkCircleCircle(a, b)) None
    else{
      val n = b.center - a.center
      val dist = n.length()


      val r = a.rad + b.rad

      if(dist > 0){

        val norm = n / dist
        val pen = r - dist

        Some(new PenetrationData2(norm, pen))
      }else{
        Some(new PenetrationData2(vec2(1,1), r)) //random direction
      }

    }
  }


  */






}
*/

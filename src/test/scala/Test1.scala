import java.util.Random

import russoul.lib.common._
import russoul.lib.common.Abstraction._
import russoul.lib.common.Implicits._
import russoul.lib.common.math.CollisionEngineF
import russoul.lib.common.math.geometry.simple.CircleOver
import russoul.lib.common.utils.Arr


import spire.math._
import spire.implicits._
import spire.algebra._

object Test1 extends App{

  val eps = 0.00001F


  def testVectorCreation(): Unit ={
    val v1 = Float3(1F,2F,3F)
    val v2 = Double2(1D,2D)
    val v3 = Int2(1,2)
  }

  def testVectorOps(): Unit ={
    val v1 = Float3(1,0,0)
    val v2 = Float3(0,1,0)

    assert(v1.x == v2.y)
    assert(v1(0) == v2(1))


  }

  def testCESOps() : Unit = {
    val v1 = Float3(1,0,0)
    val v2 = Float3(0,1,0)

    val v3 = v1 ⨯ v2

    val eps = 0.00001F

    assert(v3.distance(Float3(0,0,1)) < eps)

    val v4 = v1 * 2F + v2 * 3F + v3 * 4F

    assert(v4.distance(Float3(2F,3F,4F)) < eps )


    assert(Float2(1,2).⟂.distance(Float2(-2,1)) < eps )
    assert( (Int2(1,2) ⊗ Int2(0,-1)) == Int2(0,-2))
    assert(Double2(3D,4D).normalize.distance(Double2(0.6D, 0.8D)) < eps )
    assert( (Double4(1,2,3,4) dot Double4(-1,-2,-3,-4)) == -30D)
  }

  def testMatrixOps() : Unit = {
    val v1 = Float4(1,2,3,1)
    val m1 = Mat4F.translation(Float3(2,3,4))

    assert(v1 ⨯ m1 == Float4(3,5,7,1))

    val v2 = Float4(1,0,0,1)
    val m2 = Mat4F.rotationDeg(Float3(0,0,1), -90F)

    assert((v2 ⨯ m2).distance(Float4(0,1,0,1)) < eps )
    assert(v2 ⨯ m2 == v2 ⨯ m2.trans().trans())

    val m3 = m1 ⨯ m2

    assert(v2 ⨯ m3 == v2 ⨯ m1 ⨯ m2)

  }



  def testGeo(): Unit ={
    //Real == Double == RealD
    //Float == RealF
    //<...> == shape over Double ,example: Triangle(...)
    //<...>F == shape over Float. example: TriangleF(...)

    val i = Float3(1,0,0)
    val j = Float3(0,1,0)
    val k = Float3(0,0,1)

    val circle = CircleF(center = Float2(0F,0F), rad = 1F)
    val ray2 = Ray2(start = Double2(1D,1D), dir = Double2(1D,1D).normalize)
    val triangle = TriangleF(p1 = i, p2 = j, p3 = k)
    val orientedBoundingBox = OBB(center = Double3(0,0,0), right = Double3(1,0,0),
      up = Double3(0,1,0), extentRight = 1, extentUp = 2, extentLook = 3)//right and up must be normalized !

    val axisAlignedBoundingBox = AABBF(center = Float3(0,0,0), extent = Float3(1,2,3))


    assert(circle.translate(Float2(1,1)) == CircleF(Float2(1,1), 1))
    assert(triangle.scaleAroundBasis(2) == TriangleF(i * 2F, j * 2F, k * 2F))
    assert(axisAlignedBoundingBox.scale(3) == AABBF(Float3(0,0,0), Float3(3,6,9)))
  }

  def testCollisionEngine(): Unit ={
    val p = Float2(1,2)

    val line1 = Line2F(Float2(0,0), Float2(3,0))
    val line2 = Line2F(Float2(0,0), Float2(4,0))
    val line3 = Line2F(Float2(0,0), Float2(-3,0))


    val dist1 = CollisionEngineF.distancePoint2Line2(p, line1)
    val dist2 = CollisionEngineF.distancePoint2Line2(p, line2)
    val dist3 = CollisionEngineF.distancePoint2Line2(p, line3)

    assert(dist1 == dist2 && dist2 == dist3)



    val rec = Rectangle2F(Float2(0,0), Float2(1,1))
    val circle1 = CircleOver[Float](Float2(0,0), 1)
    val circle2 = CircleOver[Float](Float2(0,0), 2)
    val circle3 = CircleOver[Float](Float2(3,0), 1)

    assert(CollisionEngineF.checkCircleRectangle2(circle1, rec))
    assert(CollisionEngineF.checkCircleRectangle2(circle2, rec))
    assert(!CollisionEngineF.checkCircleRectangle2(circle3, rec))
  }

  testVectorCreation()
  testVectorOps()
  testCESOps()
  testMatrixOps()
  testGeo()
  testCollisionEngine()
}

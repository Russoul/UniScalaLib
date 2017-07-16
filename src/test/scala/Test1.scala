import java.util.Random

import russoul.lib.common._
import russoul.lib.common.Abstraction._
import russoul.lib.common.Implicits._
import russoul.lib.common.TypeClasses.Ring
import shapeless.Nat

object Test1 extends App{

  def testVectorCreation(): Unit ={
    val v1 = Real3F(1F,2F,3F)
    val v2 = Double2(1D,2D)
    val v3 = Int2(1,2)
  }

  def testVectorOps(): Unit ={
    val v1 = Float3(1,0,0)
    val v2 = Float3(0,1,0)

    assert(v1._0 == v2._1)
    assert(v1(0) == v2(1))


  }

  def testCESOps() : Unit = {
    val v1 = Float3(1,0,0)
    val v2 = Float3(0,1,0)

    val v3 = v1 ⨯ v2

    assert(v3 == Float3(0,0,1))

    val v4 = v1 * 2F + v2 * 3F + v3 * 4F

    assert(v4 == Float3(2F,3F,4F))

    assert(Float2(1,2).⟂ == Float2(-2,1))
    assert( (Int2(1,2) ⊗ Int2(0,-1)) == Int2(0,-2))
    assert(Double2(3D,4D).normalize() == Double2(0.6D, 0.8D))
    assert( (Double4(1,2,3,4) dot Double4(-1,-2,-3,-4)) == -30D)
  }

  def testMatrixOps() : Unit = {
    val v1 = Float4(1,2,3,0)
    val m1 = Mat4F.translation(Float3(2,3,4))

    assert(v1 * m1 == Float4(3,5,7,0))

    val v2 = Float4(1,0,0,0)
    val m2 = Mat4F.rotationDeg(Float3(0,0,1), -90F)

    assert(v1 * m2 == Float4(0,1,0,0))
    assert(v1 * m2 == m2.transpose() * v1)

    val m3 = m2 ⨯ m1

    assert(v2 * m3 == v2 * m1 * m2)

  }

  def testAbstraction(): Unit ={

    def combine[Vector[_,_ <: Nat], R, Dim <: Nat]
    (args: Vector[R,Dim]*)(implicit ring: Ring[R], t1: T1[R,Vector,Dim], ces: Module[Vector,R,Dim]): Vector[R,Dim] ={
      args.foldRight(ces.zero){_ + _}
    }

    assert(combine(Int2(1,2), Int2(2,4), Int2(4,5), Int2(7,1)) == Int2(14, 12))
    assert(combine(Float3(1,2,3), Float3(2,4,-3), Float3(4,5,0), Float3(7,1,0)) == Float3(14, 12, 0))
    assert(combine(Double4(1,1,2,3), Double4(1,2,4,-3), Double4(1,4,5,0), Double4(1,7,1,0)) == Double4(4, 14, 12, 0))

  }

  testVectorCreation()
  testVectorOps()
  testCESOps()
  testMatrixOps()
  testAbstraction()
}

import java.util.Random

import Russoul.lib.common.Implicits._
import Russoul.lib.common.Int2
import Russoul.lib.common.Ops.Container2Ops
import Russoul.lib.common.TypeClasses.{ArrayIsContainerAny, CommutativeGroup, Container2, Vec2IsContainer2}
import Russoul.lib.common.math.Solver
import Russoul.lib.common.math.algebra.Vec2
import Russoul.lib.common.utils._


object MainTest extends App{

  def testPolynoms: Unit = {
    val res1 = Solver.findRealRootsPolynomial3(1D, -3D, 21D, -19D)
    res1.foreach(println)

    val res2 = Solver.findRealRootsPolynomial4(1D, 3D, 3D, -1D, -6D)
    res2 match {
      case Some(x) => x.foreach(println)
      case None => println("no real roots !")
    }

  }

  def testNewtonMethod: Unit = {
    val f = (x: Float) =>  (x+3)*(x+4) //x^2 + 2x - 8
    val dfdx = (x: Float) => 2*x + 2
    val x0 = -2.9F
    val solved = Solver.NewtonMethod.solve(x0, f, dfdx)
    println(solved)
  }

  def testAdhocPolymorphism(): Unit =
  {

    def sum[Con2,@specialized(Float) T](con2: Con2)(implicit i : Container2[T, Con2], group: CommutativeGroup[T]): T ={
      con2.x + con2.y
    }

    implicit val cont = new Vec2IsContainer2[Float]
    //implicit val contArray = new ArrayIsContainerAny[Float]

    println(sum(Vec2(1F,2F)))
    println(sum(Array(1F,2F)))
    println(sum(Array(1F,2F)))

  }

  testAdhocPolymorphism()
}

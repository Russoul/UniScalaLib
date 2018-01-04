import russoul.lib.common.{FCircle, FRectangle2, Float2}
import russoul.lib.common._
import russoul.lib.common.Abstraction._
import russoul.lib.common.Implicits._
import russoul.lib.common.math.algebra.Mat
import russoul.lib.common.math.geometry.simple.Line2Over
import russoul.lib.common.utils.Arr
import spire.math._
import spire.implicits._
import spire.algebra._
import metal._
import syntax._
import metal._
import spire.std.ArrayCoordinateSpace
import syntax._

object Spec extends App{


  def sampleIntersectionBrute(line : Line2Over[Float], n: Int, f : FShape2) : Float2 = {
    val ext = line.end - line.start

    var bestAbs = 100000000F //placeholder TODO
    var bestPoint = nil[Float2]

    for(i <- 0 to n){
      val point = line.start + (i.toFloat / n.toFloat) *: ext
      //val den = f.density(point)
      val den = 25F
      val abs = Math.abs(den)

      if(abs < bestAbs){
        bestAbs = abs
        bestPoint = point
      }

    }

    bestPoint
  }

  def specializationTest(): Float ={



    val offset = Float2(0.1F, 0.1F)
    val circle1 = FCircle(Float2(4,8) + offset, 2F)
    val circle2 = FCircle(Float2(8,8) + offset, 5F)
    val circle3 = FCircle(Float2(4,4) + offset, 2F)
    val circle4 = FCircle(Float2(8,12) + offset,4F)
    val circle5 = FCircle(Float2(8,6) + offset,1.1F)

    val debugCircle = FCircle(Float2(2.8000002F, 11.203231F), 1.5F)

    val rec = FRectangle2(Float2(8,10.8F) + offset, Float2(1F,3F))

    val shape = ((((circle1 | circle2) | rec ) - circle3 - circle4 - circle5) | rec)


    val generator : Function1R[Float2, Float] = shape.density

    val t = generator(Float2(1,1))

    var t4 = 2F

    for(i <- 0 until 10){
      t4 += generator(Float2(i,i))
    }


    var m = 1F

    var j = 0
    while(j < 100000){
      var i = 0
      while(i < 1000000000){
        val v = Vec3(1F,2F,4F)
        m += v(1)
        i += 1
      }
      j += 1
    }


    m
  }

  specializationTest()
}

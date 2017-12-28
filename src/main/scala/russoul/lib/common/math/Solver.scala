package russoul.lib.common.math

import java.lang

import russoul.lib.common._
import russoul.lib.common.TypeClasses._
import russoul.lib.common.Implicits._
import russoul.lib.common.math.algebra.Row

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra._
import spire.math._
import spire.implicits._

/**
  * Created by russoul on 13.05.17.
  */
object Solver
{

  /**
    * aX + b = 0
    */
  @inline def findRealRootsPolynomial1[@specialized(Float,Double,Int) A](a:A, b:A)(implicit ev: Field[A]): Option[A] ={
    if(a != 0) Some(-b/a) else None
  }


  @inline def findDiscriminantOfPolynomial2[@specialized(Float,Double,Int) A](a:A, b:A, c:A)(implicit ev: Field[A]) : A =
  {
    b * b - ev.fromDouble(4D) * a * c
  }

  /**
    * aX² + bX + c = 0, a ≠ 0
    */
  def findRealRootsPolynomial2[@specialized(Float,Double,Int) A : ClassTag](a:A, b:A, c:A)(implicit ev: Field[A], order : Order[A], nroot : NRoot[A]) : Option[Vec2[A]] =
  {
    val disc = findDiscriminantOfPolynomial2(a,b,c)

    if(disc < ev.zero){
      None
    }else{
      val sqrt = nroot.sqrt(disc)
      val r1 = (- b + sqrt)/ev.fromDouble(2D)/a
      val r2 = (- b - sqrt)/ev.fromDouble(2D)/a

      Some(Vec2(r1, r2))
    }
  }

  /**
    * X³ + pX + q = 0
    */
  def findRealRootsCanonicalPolynomial3[@specialized(Float,Double,Int) A : ClassTag](p : A, q: A)(implicit ev: Field[A], order : Order[A], nroot : NRoot[A], trig : Trig[A]): Array[A] = {

    val three = ev.fromDouble(3D)
    val two = ev.fromDouble(2D)
    val one = ev.fromDouble(1D)
    
    val Q = nroot.fpow(p / three, three) + nroot.fpow(q/two, two)
    val sqrtQ = nroot.sqrt(Q)
    val alpha = nroot.nroot(-q/two + sqrtQ, 3)
    val beta = nroot.nroot(-q/two - sqrtQ, 3)

    val sum = alpha + beta



    if(Q > ev.zero){ //1 Real, 2 Complex conjugated roots
      Array(sum)
    }else if(Q === ev.zero){
      val r = -sum/two

      Array(alpha + beta, r, r)
    }else{
      val phi = if(q < ev.zero){
        trig.atan(nroot.sqrt(-Q)/(-q/two))
      }else if(q > ev.zero){
        trig.atan(nroot.sqrt(-Q)/(-q/two)) + ev.fromDouble(Math.PI)
      }else{
        ev.fromDouble(Math.PI / 2D)
      }
      val r1 = nroot.sqrt(-p/three) * trig.cos(phi/three) * two
      val r2 = nroot.sqrt(-p/three) * trig.cos(phi/three + ev.fromDouble(2D * Math.PI / 3D)) * two
      val r3 = nroot.sqrt(-p/three) * trig.cos(phi/three + ev.fromDouble(4D * Math.PI / 3D)) * two

      Array(r1,r2,r3)
    }
  }

  /**
    * aX³ + bX² + cX + d = 0, a ≠ 0
    */
  def findRealRootsPolynomial3[@specialized(Float,Double,Int) A : ClassTag](a: A, b: A, c: A, d: A)(implicit ev: Field[A], order : Order[A], nroot : NRoot[A], trig : Trig[A]): Array[A] = {

    val three = ev.fromDouble(3D)
    val two = ev.fromDouble(2D)
    val nine = ev.fromDouble(9D)
    val twentySeven = ev.fromDouble(27D)
    
    val p = (a*c*three - b*b)/(a*a*three)
    val q = (b*b*b*two - a*b*c*nine + a*a*d*twentySeven)/(a*a*a*twentySeven)

    val findCanonical = findRealRootsCanonicalPolynomial3(p,q)
    val res = new Array[A](findCanonical.size)
    for(i <- 1 to findCanonical.size){
      res(i-1) = findCanonical(i) - b/(a*three)
    }

    res
  }

  /**
    * aX⁴ + bX³ + cX² + dX + e = 0, a ≠ 0
    */
  def findRealRootsPolynomial4[@specialized(Float,Double,Int) A : ClassTag](a: A, b: A, c: A, d: A, e: A)(implicit ev: Field[A], order : Order[A], nroot : NRoot[A], trig : Trig[A]): Option[Array[A]] = {

    val four = ev.fromDouble(4D)
    val three = ev.fromDouble(3D)
    val two = ev.fromDouble(2D)
    val one = ev.fromDouble(1D)
    
    
    val A = b/a
    val B = c/a
    val C = d/a
    val D = e/a


    val y0 = findRealRootsPolynomial3[A](one, -B, A*C - D * four, -A*A*D + B*D * four - C*C)

    val y1 = y0(0)


    val n1 = nroot.sqrt(A*A/four - B + y1)
    val n2 = nroot.sqrt(y1*y1/four - D)

    val r1 = findRealRootsPolynomial2[A](one, A/two - n1, y1/two - n2)
    val r2 = findRealRootsPolynomial2[A](one, A/two + n1, y1/two + n2)



    r1 match{
      case None =>
        r2 match{
          case None =>
            None
          case some =>
            Some(Array(some.get(0), some.get(1)))
        }
      case some =>
        r2 match{
          case None =>
            Some(Array(some.get.x, some.get.y))
          case also =>
            Some(Array(some.get.x, some.get.y, also.get.x, also.get.y))
        }
    }
  }


  object NewtonMethod {

    final val STANDARD_ACCURACY = 1e-6F

    @tailrec
    def solve(x0: Float, f: Float => Float, dfdx: Float => Float, e: Float = STANDARD_ACCURACY): Float = {
      val x1 = x0 - f(x0) / dfdx(x0)
      if (lang.Math.abs(x1 - x0) < e) x1
      else solve(x1, f, dfdx, e)
    }


  }


}

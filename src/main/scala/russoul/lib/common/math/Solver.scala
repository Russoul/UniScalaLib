package russoul.lib.common.math

import java.lang

import russoul.lib.common.{Real, Vec2, tbsp}
import russoul.lib.common.TypeClasses._
import russoul.lib.common.Implicits._
import russoul.lib.common.math.algebra.Vec

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * Created by russoul on 13.05.17.
  */
object Solver
{

  /**
    * aX + b = 0
    */
  @inline def findRealRootsPolynomial1[@tbsp A](a:A, b:A)(implicit ev: Field[A]): Option[A] ={
    if(a != 0) Some(-b/a) else None
  }


  @inline def findDiscriminantOfPolynomial2[@tbsp A](a:A, b:A, c:A)(implicit ev: Field[A] with ConvertibleFromDouble[A]) : A =
  {
    b * b - 4D.as[A] * a * c
  }

  /**
    * aX² + bX + c = 0, a ≠ 0
    */
  def findRealRootsPolynomial2[A : ClassTag](a:A, b:A, c:A)(implicit ev: Field[A] with Orderable[A] with ConvertibleFromDouble[A] with Euclidean[A]) : Option[Vec2[A]] =
  {
    val disc = findDiscriminantOfPolynomial2(a,b,c)

    if(disc < ev.zero){
      None
    }else{
      val sqrt = ev.sqrt(disc)
      val r1 = (- b + sqrt)/2D.as[A]/a
      val r2 = (- b - sqrt)/2D.as[A]/a

      Some(Vec2(r1, r2))
    }
  }

  /**
    * X³ + pX + q = 0
    */
  def findRealRootsCanonicalPolynomial3[A : ClassTag](p : A, q: A)(implicit ev: Field[A] with Orderable[A] with ConvertibleFromDouble[A] with Euclidean[A] with Trig[A]): Array[A] = {

    val Q = ev.pow(p / 3D.as[A], 3D.as[A]) + ev.pow(q/2D.as[A], 2D.as[A])
    val sqrtQ = ev.sqrt(Q)
    val alpha = ev.cbrt(-q/2D.as[A] + sqrtQ)
    val beta = ev.cbrt(-q/2D.as[A] - sqrtQ)

    val sum = alpha + beta



    if(Q > 0D.as[A]){ //1 Real, 2 Complex conjugated roots
      Array(sum)
    }else if(Q =? 0D.as[A]){
      val r = -sum/2D.as[A]

      Array(alpha + beta, r, r)
    }else{
      val phi = if(q < 0D.as[A]){
        ev.atan(ev.sqrt(-Q)/(-q/2D.as[A]))
      }else if(q > 0D.as[A]){
        ev.atan(ev.sqrt(-Q)/(-q/2D.as[A])) + ev.fromDouble(Math.PI)
      }else{
        ev.fromDouble(Math.PI / 2D)
      }
      val r1 = ev.sqrt(-p/3D.as[A]) * ev.cos(phi/3D.as[A]) * 2D.as[A]
      val r2 = ev.sqrt(-p/3D.as[A]) * ev.cos(phi/3D.as[A] + ev.fromDouble(2D * Math.PI / 3D)) * 2D.as[A]
      val r3 = ev.sqrt(-p/3D.as[A]) * ev.cos(phi/3D.as[A] + ev.fromDouble(4D * Math.PI / 3D)) * 2D.as[A]

      Array(r1,r2,r3)
    }
  }

  /**
    * aX³ + bX² + cX + d = 0, a ≠ 0
    */
  def findRealRootsPolynomial3[A : ClassTag](a: A, b: A, c: A, d: A)(implicit ev: Field[A] with Orderable[A] with ConvertibleFromDouble[A] with Euclidean[A] with Trig[A]): Array[A] = {
    val p = (a*c*3D.as[A] - b*b)/(a*a*3D.as[A])
    val q = (b*b*b*2D.as[A] - a*b*c*9D.as[A] + a*a*d*27D.as[A])/(a*a*a*27D.as[A])

    val findCanonical = findRealRootsCanonicalPolynomial3(p,q)
    val res = new Array[A](findCanonical.size)
    for(i <- 1 to findCanonical.size){
      res(i-1) = findCanonical(i) - b/(a*3D.as[A])
    }

    res
  }

  /**
    * aX⁴ + bX³ + cX² + dX + e = 0, a ≠ 0
    */
  def findRealRootsPolynomial4[A : ClassTag](a: A, b: A, c: A, d: A, e: A)(implicit ev: Field[A] with Orderable[A] with ConvertibleFromDouble[A] with Euclidean[A] with Trig[A]): Option[Array[A]] = {

    val A = b/a
    val B = c/a
    val C = d/a
    val D = e/a


    val y0 = findRealRootsPolynomial3[A](1D.as[A], -B, A*C - D * 4D.as[A], -A*A*D + B*D * 4D.as[A] - C*C)

    val y1 = y0(0)


    val n1 = ev.sqrt(A*A/4D.as[A] - B + y1)
    val n2 = ev.sqrt(y1*y1/4D.as[A] - D)

    val r1 = findRealRootsPolynomial2[A](1D.as[A], A/2D.as[A] - n1, y1/2D.as[A] - n2)
    val r2 = findRealRootsPolynomial2[A](1D.as[A], A/2D.as[A] + n1, y1/2D.as[A] + n2)



    r1 match{
      case None =>
        r2 match{
          case None =>
            None
          case some =>
            Some(Array(some.get.x, some.get.y))
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

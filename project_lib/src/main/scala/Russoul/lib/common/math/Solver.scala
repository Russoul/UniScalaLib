package Russoul.lib.common.math

import Russoul.lib.common.Real
import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.Implicits._
import Russoul.lib.common.math.algebra.{Vec, Vec2}

import scala.reflect.ClassTag

/**
  * Created by russoul on 13.05.17.
  */
object Solver
{

  /**
    * a1X + a0 = 0
    */
  @inline def findRealRootsPolynomial1[@specialized A](a1:A, a0:A)(implicit ev: Field[A]): Option[A] ={
    if(a1 != 0) Some(-a0/a1) else None
  }


  @inline def findDiscriminantOfPolynomial2[@specialized A](a:A, b:A, c:A)(implicit ev: Field[A] with ConvertibleFromDouble[A]) : A =
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
  def findRealRootsCanonicalPolynomial3[A : ClassTag](p : A, q: A)(implicit ev: Field[A] with Orderable[A] with ConvertibleFromDouble[A] with Euclidean[A] with Trig[A]): Vec[A] = {

    val Q = ev.pow(p / 3D.as[A], 3D.as[A]) + ev.pow(p/2D.as[A], 2D.as[A])
    val sqrtQ = ev.sqrt(Q)
    val alpha = ev.pow(-q/2D.as[A] + sqrtQ, 1D.as[A]/3D.as[A])
    val beta = ev.pow(-q/2D.as[A] - sqrtQ, 1D.as[A]/3D.as[A])

    val sum = alpha + beta

    if(Q > 0D.as[A]){ //1 Real, 2 Complex conjugated roots

      Vec(sum)
    }else if(Q =? 0D.as[A]){
      val r = -sum/2D.as[A]

      Vec(alpha + beta, r, r)
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

      Vec(r1,r2,r3)
    }
  }

  /**
    * aX³ + bX² + cX + d = 0, a ≠ 0
    */
  def findRealRootsPolynomial3[A : ClassTag](a: A, b: A, c: A, d: A)(implicit ev: Field[A] with Orderable[A] with ConvertibleFromDouble[A] with Euclidean[A] with Trig[A]): Vec[A] = {
    val p = (a*c*3D.as[A] - b*b)/(a*a*3D.as[A])
    val q = (b*b*b*2D.as[A] - a*b*c*9D.as[A] + a*a*d*27D.as[A])/(a*a*a*27D.as[A])

    val findCanonical = findRealRootsCanonicalPolynomial3(p,q)
    val res = new Array[A](findCanonical.dim())
    for(i <- 1 to findCanonical.dim()){
      res(i-1) = findCanonical(i) - b/(a*3D.as[A])
    }

    Vec[A](res)//TODO does not work, fix : http://math.intemodino.com/ru/algebra/equations/cardano%27s-formula-for-solving-cubic-equations.html
  }



}

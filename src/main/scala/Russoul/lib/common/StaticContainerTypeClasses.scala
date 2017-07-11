package Russoul.lib.common

import Russoul.lib.common.TypeClasses._
import Russoul.lib.common.math.algebra._
import shapeless.{<:!<, Nat}
import shapeless.ops.nat
import shapeless.ops.nat.Diff.Aux
import shapeless.ops.nat.{Diff, GT, Sum}
import shapeless.ops.nat.Sum.Aux

import scala.reflect.ClassTag

/**
  * Created by russoul on 09.07.2017.
  */
object StaticContainerTypeClasses {

  /*class Vec2IsStaticVector[@sp T : ClassTag] extends StaticVector[T,Vec2[T],Nat._2, Vec3[T], Unit]{

    override def get[Index <: Nat](con: Vec2[T], index: Index)(implicit ev: shapeless.<:!<[Nat._2, Index], toInt: nat.ToInt[Index]): T = {
      con(toInt())
    }

    override def make(args: T*): Vec2[T] = {
      new Vec2[T](args(0),args(1))
    }

    override def lower[C <: Nat]()(implicit sizeMinusOne: Diff.Aux[Nat._2, Nat._1, C], gtTwo: GT[Nat._2, Nat._2]): Unit = {}

    override def higher[C <: Nat](self: Vec2[T], tail : T)(implicit sizePlusOne: Sum.Aux[Nat._2, Nat._1, C]): Vec3[T] = {
      Vec3[T](self(0), self(1), tail)
    }
  }

  class Vec3IsStaticVector[@sp T : ClassTag] extends StaticVector[T,Vec3[T],Nat._3, Vec4[T], Vec2[T]]{

    override def get[Index <: Nat](con: Vec3[T], index: Index)(implicit ev: shapeless.<:!<[Nat._3, Index], toInt: nat.ToInt[Index]): T = {
      con(toInt())
    }

    override def make(args: T*): Vec3[T] = {
      new Vec3[T](args(0),args(1),args(2))
    }

    override def lower[C <: Nat]()(implicit sizeMinusOne: Diff.Aux[Nat._3, Nat._1, C], gtTwo: GT[Nat._3, Nat._2]): Vec2[T] = {
      self
    }

    override def higher[C <: Nat](self: Vec3[T], tail: T)(implicit sizePlusOne: Sum.Aux[Nat._3, Nat._1, C]): Vec4[T] = ???
  }

  class Vec4IsStaticVector[@sp T : ClassTag] extends StaticVector[T,Vec4[T],Nat._4]{

    override def get[Index <: Nat](con: Vec4[T], index: Index)(implicit ev: shapeless.<:!<[Nat._4, Index], toInt: nat.ToInt[Index]): T = {
      con(toInt())
    }

    override def make(args: T*): Vec4[T] = {
      new Vec4[T](args(0),args(1),args(2),args(3))
    }
  }

  //paired with Vec2
  class Mat2IsStaticSquareMatrix[@sp T: ClassTag](spaceArg : CanonicalEuclideanSpaceOverField[Vec2[T], T, Nat._2]) extends StaticSquareMatrix[T, Vec2[T], Mat2[T], Nat._2]{
    override implicit val vector: StaticVector[T, Vec2[T], Nat._2] = _
    override implicit val space: CanonicalEuclideanSpaceOverField[Vec2[T], T, Nat._2] = spaceArg

    override protected def get(mat: Mat2[T], i: Int, j: Int): T = mat.apply(i,j)

    override def make(args: T*): Mat2[T] = Mat2[T](args : _*)
  }

  //paired with Vec4
  class Mat4IsStaticSquareMatrix[@sp T: ClassTag](spaceArg : CanonicalEuclideanSpaceOverField[Vec4[T], T, Nat._4]) extends StaticSquareMatrix[T, Vec4[T], Mat4[T], Nat._4]{
    override implicit val vector: StaticVector[T, Vec4[T], Nat._4] = _
    override implicit val space: CanonicalEuclideanSpaceOverField[Vec4[T], T, Nat._4] = spaceArg

    override protected def get(mat: Mat4[T], i: Int, j: Int): T = mat.apply(i,j)

    override def make(args: T*): Mat4[T] = Mat4[T](args : _*)
  }

  class Tuple2IsStaticVector[@sp T: ClassTag] extends StaticVector[T,(T,T), Nat._2]{
    override def get[Index <: Nat](con: (T, T), index: Index)(implicit ev: shapeless.<:!<[Nat._2, Index], toInt: nat.ToInt[Index]): T = {
      con.productElement(toInt()).asInstanceOf[T]
    }

    override def make(args: T*): (T, T) = {
      (args(0), args(1))
    }
  }

  class Tuple3IsStaticVector[@sp T: ClassTag] extends StaticVector[T,(T,T,T), Nat._3]{
    override def get[Index <: Nat](con: (T, T, T), index: Index)(implicit ev: shapeless.<:!<[Nat._3, Index], toInt: nat.ToInt[Index]): T = {
      con.productElement(toInt()).asInstanceOf[T]
    }

    override def make(args: T*): (T, T, T) = {
      (args(0), args(1), args(2))
    }
  }

  class Tuple4IsStaticVector[@sp T: ClassTag] extends StaticVector[T,(T,T,T,T), Nat._4]{
    override def get[Index <: Nat](con: (T, T, T, T), index: Index)(implicit ev: shapeless.<:!<[Nat._4, Index], toInt: nat.ToInt[Index]): T = {
      con.productElement(toInt()).asInstanceOf[T]
    }

    override def make(args: T*): (T, T, T, T) = {
      (args(0), args(1), args(2), args(3))
    }
  }*/


  class VecIsStaticVector[@sp T : ClassTag] extends StaticVector[T, Vec]{
    override val factory: AlgebraicTypeFactory[T, Vec, _] = new DefaultAlgebraicFactory[T]
  }

  class MatIsStaticMatrix[@sp T : ClassTag] extends StaticSquareMatrix[T, Vec, Mat]{
    override val factory: AlgebraicTypeFactory[T, Vec, Mat] = new DefaultAlgebraicFactory[T]
  }

  

}

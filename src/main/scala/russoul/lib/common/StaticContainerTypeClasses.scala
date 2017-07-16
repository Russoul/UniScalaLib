package russoul.lib.common

import russoul.lib.common.TypeClasses._
import russoul.lib.common.math.algebra._
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

  /*class Vec2IsStaticVector[@tbsp T : ClassTag] extends StaticVector[T,Vec2[T],Nat._2, Vec3[T], Unit]{

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

  class Vec3IsStaticVector[@tbsp T : ClassTag] extends StaticVector[T,Vec3[T],Nat._3, Vec4[T], Vec2[T]]{

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

  class Vec4IsStaticVector[@tbsp T : ClassTag] extends StaticVector[T,Vec4[T],Nat._4]{

    override def get[Index <: Nat](con: Vec4[T], index: Index)(implicit ev: shapeless.<:!<[Nat._4, Index], toInt: nat.ToInt[Index]): T = {
      con(toInt())
    }

    override def make(args: T*): Vec4[T] = {
      new Vec4[T](args(0),args(1),args(2),args(3))
    }
  }

  //paired with Vec2
  class Mat2IsStaticSquareMatrix[@tbsp T: ClassTag](spaceArg : CanonicalEuclideanSpaceOverField[Vec2[T], T, Nat._2]) extends StaticSquareMatrix[T, Vec2[T], Mat2[T], Nat._2]{
    override implicit val vector: StaticVector[T, Vec2[T], Nat._2] = _
    override implicit val space: CanonicalEuclideanSpaceOverField[Vec2[T], T, Nat._2] = spaceArg

    override protected def get(mat: Mat2[T], i: Int, j: Int): T = mat.apply(i,j)

    override def make(args: T*): Mat2[T] = Mat2[T](args : _*)
  }

  //paired with Vec4
  class Mat4IsStaticSquareMatrix[@tbsp T: ClassTag](spaceArg : CanonicalEuclideanSpaceOverField[Vec4[T], T, Nat._4]) extends StaticSquareMatrix[T, Vec4[T], Mat4[T], Nat._4]{
    override implicit val vector: StaticVector[T, Vec4[T], Nat._4] = _
    override implicit val space: CanonicalEuclideanSpaceOverField[Vec4[T], T, Nat._4] = spaceArg

    override protected def get(mat: Mat4[T], i: Int, j: Int): T = mat.apply(i,j)

    override def make(args: T*): Mat4[T] = Mat4[T](args : _*)
  }

  class Tuple2IsStaticVector[@tbsp T: ClassTag] extends StaticVector[T,(T,T), Nat._2]{
    override def get[Index <: Nat](con: (T, T), index: Index)(implicit ev: shapeless.<:!<[Nat._2, Index], toInt: nat.ToInt[Index]): T = {
      con.productElement(toInt()).asInstanceOf[T]
    }

    override def make(args: T*): (T, T) = {
      (args(0), args(1))
    }
  }

  class Tuple3IsStaticVector[@tbsp T: ClassTag] extends StaticVector[T,(T,T,T), Nat._3]{
    override def get[Index <: Nat](con: (T, T, T), index: Index)(implicit ev: shapeless.<:!<[Nat._3, Index], toInt: nat.ToInt[Index]): T = {
      con.productElement(toInt()).asInstanceOf[T]
    }

    override def make(args: T*): (T, T, T) = {
      (args(0), args(1), args(2))
    }
  }

  class Tuple4IsStaticVector[@tbsp T: ClassTag] extends StaticVector[T,(T,T,T,T), Nat._4]{
    override def get[Index <: Nat](con: (T, T, T, T), index: Index)(implicit ev: shapeless.<:!<[Nat._4, Index], toInt: nat.ToInt[Index]): T = {
      con.productElement(toInt()).asInstanceOf[T]
    }

    override def make(args: T*): (T, T, T, T) = {
      (args(0), args(1), args(2), args(3))
    }
  }*/

  //TODO require it to be a field ?
  class TIsTensor0[@tbsp T : ClassTag] extends Tensor0[T]

  class VecIsTensor1[@tbsp T : ClassTag, A1 <: Nat] extends Tensor1[T, Vec, A1]{
    override def make(args: T*)(implicit ev1: nat.ToInt[A1]): Vec[T, A1] = Vec(args : _*)
    override def get(a: Vec[T, A1], i: Int): T = a(i)
  }

  class MatIsTensor2[@tbsp T : ClassTag, A1 <: Nat, A2 <: Nat] extends Tensor2[T,Mat,A1,A2]{
    override def make(args: T*)(implicit ev1: nat.ToInt[A1], ev2: nat.ToInt[A2]): Mat[T, A1, A2] = Mat[T,A1,A2](args : _*)
    override def get(a: Mat[T, A1, A2], i: Int, j: Int): T = a(i,j)
  }


  

}

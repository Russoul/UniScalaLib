package Russoul.lib.common

import Russoul.lib.common.TypeClasses.StaticVector
import Russoul.lib.common.math.algebra.{Vec2, Vec3, Vec4}
import shapeless.Nat
import shapeless.ops.nat

import scala.reflect.ClassTag

/**
  * Created by russoul on 09.07.2017.
  */
object StaticContainerTypeClasses {

  class Vec2IsStaticVector[@sp T : ClassTag] extends StaticVector[T,Vec2[T],Nat._2]{

    override def get[Index <: Nat](con: Vec2[T], index: Index)(implicit ev: shapeless.<:!<[Nat._2, Index], toInt: nat.ToInt[Index]): T = {
      con(toInt())
    }

    override def make(args: T*): Vec2[T] = {
      new Vec2[T](args(0),args(1))
    }
  }

  class Vec3IsStaticVector[@sp T : ClassTag] extends StaticVector[T,Vec3[T],Nat._3]{

    override def get[Index <: Nat](con: Vec3[T], index: Index)(implicit ev: shapeless.<:!<[Nat._3, Index], toInt: nat.ToInt[Index]): T = {
      con(toInt())
    }

    override def make(args: T*): Vec3[T] = {
      new Vec3[T](args(0),args(1),args(2))
    }
  }

  class Vec4IsStaticVector[@sp T : ClassTag] extends StaticVector[T,Vec4[T],Nat._4]{

    override def get[Index <: Nat](con: Vec4[T], index: Index)(implicit ev: shapeless.<:!<[Nat._4, Index], toInt: nat.ToInt[Index]): T = {
      con(toInt())
    }

    override def make(args: T*): Vec4[T] = {
      new Vec4[T](args(0),args(1),args(2),args(3))
    }
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
  }

  

}

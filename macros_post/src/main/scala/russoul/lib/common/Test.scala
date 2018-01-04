package russoul.lib.common

import cats.Show
import russoul.lib.macros.{Ref, array}
import cats.syntax._
import cats.instances._
import cats.implicits._

import scala.reflect.ClassTag

object Test extends App{

  /*case class Person(name : String, age : Int)

  def chooseAgeBelowGiven(list : List[Person], age : Int)(implicit requirement : Ref["age > 0"]){
    list.filter(_.age < age)
  }

  chooseAgeBelowGiven(List(Person("a", 10), Person("b", 18), Person("c", 20)), -1)*/

  implicit class ApplicationLeft[A](a : A){
    def !:[B](f : A => B) = f(a)
  }

  implicit class ApplicationRight[A](a : A){
    def |>[B](f : A => B) = f(a)
  }

  implicit class Curry[A,B,C](f : (A, B) => C){
    def >> : A => B => C = f.curried
  }



  def show[T](v : T)(implicit show : Show[T]) = show.show(v)
  val putStrLn = (str : String) => println(str)

  def sum2(x : Int, y : Int) = x + y
  val plus = (x : Int) => (y : Int) => x + y


  1 |> plus(3) |> show[Int] _ |> putStrLn
  putStrLn !: show[Int] _ !: plus(3) !: 1
  putStrLn(show(plus(3)(1)))


  val ar = array!(2F, 3F, 4F)

  def arr_test[T : ClassTag](x : T, y : T, z : T) : Array[T] = array!(x,y,z)

}
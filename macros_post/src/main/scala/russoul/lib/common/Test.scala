package russoul.lib.common

import russoul.lib.macros.{Ref}

object Test extends App{

  case class Person(name : String, age : Int)

  def chooseAgeBelowGiven(list : List[Person], age : Int)(implicit requirement : Ref["age > 0"]){
    list.filter(_.age < age)
  }

  chooseAgeBelowGiven(List(Person("a", 10), Person("b", 18), Person("c", 20)), -1)

}
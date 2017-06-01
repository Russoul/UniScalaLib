package Russoul.lib.common.lang

import scala.annotation.Annotation

/**
  * Created by russoul on 01.06.2017.
  */

/**
  * this annotation is used on objects(classes, methods, functions, code blocks, etc)
  * if they contain code that does not perform checks on given input
  * primary use case: getting extra performance
  *
  * examples:
  *
  * def sum2(a:Array[Int], b:Array[Int]) ={
  *   Array(a(0) + b(0), a(1) + b(1))
  * }
  * as you see this function does not perform common sense checks like bounds check or check for
  * correct size of input arrays (a.size == b.size && a.size == 2)
  *
  */
class unchecked extends Annotation{

}

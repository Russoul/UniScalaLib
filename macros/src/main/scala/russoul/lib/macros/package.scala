package russoul.lib


import java.util.regex.Pattern

import machinist.{DefaultOperatorNames, DefaultOps, Ops}
import singleton.ops.XString

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.macros.whitebox
import scala.annotation.{StaticAnnotation, elidable}
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._

/**
  * Created by russoul on 05.07.2017.
  */
package object macros {




  trait OperatorNames {

    val operatorNames: Map[String, String] = Map(
      // Eq (=== $eq$eq$eq)
      ("$eq$eq$eq", "equiv"),
      ("$eq$bang$eq", "nequiv"),
      ("_0", "_0"),
      ("_1", "_1"),
      ("_2", "_2"),
      ("_3", "_3"),
      ("cross","map"), //cross
      ("$u2A2F","map"), //cross
      ("$u27C2","map"), //perpendicular
      ("ortho", "map"),
      ("$u2297","elem"), //by element product
      ("elem","elem"),
      ("x", "x"),
      ("y", "y"),
      ("z", "z"),
      ("w", "w"),
      ("as", "fromDouble")
    ) ++ (DefaultOps.operatorNames - "$eq$eq$eq")
  }

  object Enricher extends Ops with OperatorNames

  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox.Context

  object vec {
    def apply(xs: Any*) : Any = macro at_impl
    def at_impl(c: Context)(xs: c.Expr[Any]*) : c.Expr[Any] = {
      import c.universe._

      // First let's show that we can recover the types:
      //println(xs.map(_.tree.tpe.widen))
      val tpe = xs(0).tree.tpe.widen
      val filtered = xs.filter(_.tree.tpe.widen == tpe)
      if(filtered.size != xs.size){
        c.abort(c.enclosingPosition, "Varargs must contain elements of the same type !")
      }


      val tpeTree = tq"$tpe"
      val natTpeTree = c.parse(s"""${xs.size.toString}""")
      val tree = q"russoul.lib.common.math.algebra.Vec.apply[$tpeTree,$natTpeTree](..$xs)"
      c.Expr(tree)
    }
  }


  case class Ref[Str <: XString]()

  class ref extends StaticAnnotation {
    def macroTransform(annottees: Any*) : Any = macro ref.impl
  }

  object ref {

    var REFINE: Boolean = true

    //val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
    def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      val result = {
        annottees.map(_.tree).toList match {

          case q"$mods def $methodName[..$tpes](...$args): $returnType = { ..$body }" :: Nil => {
            //println(args.toString())
            val pattern1 = """Ref\["\s*(.*)\s*\|\s*(.*)"\.type\]""".r.unanchored
            val pattern2 = """Ref\["\s*(.*)"\.type\]""".r.unanchored



            if(!REFINE){

              val argsResLast = args.last.filter(arg => pattern2.findFirstIn(arg.toString()).isEmpty)
              val argsRes = args.init :+ argsResLast

              q"""$mods def $methodName[..$tpes](...$argsRes): $returnType =  {..$body}"""
            }else{
              val out = c.freshName()
              val bodyToString = q"""{..$body}.toString()"""

              args.toString() match{
                case pattern1(input, output) =>
                  val argsResLast = args.last.filter(arg => pattern1.findFirstIn(arg.toString()).isEmpty)
                  val argsRes = args.init :+ argsResLast

                  val outputBind = output.replace("$", out)
                  /* println(
                     s"""
                       input: $input
                       output: $output
                     """.stripMargin)*/

                  val tree = q"""$mods def $methodName[..$tpes](...$argsRes): $returnType =  {
                  assert(${c.parse(input)}, ${"Input constraint not kept: " + input})
                  val ${c.parse(out)} = {..$body}
                  assert(${c.parse(outputBind)}, ${"Output constraint (" + output + ") not kept on output ("} + $bodyToString + ")")
                  ${c.parse(out)}
                }"""
                  println(showCode(tree))
                  tree
                case pattern2(input) =>

                  val argsResLast = args.last.filter(arg => pattern2.findFirstIn(arg.toString()).isEmpty)
                  val argsRes = args.init :+ argsResLast

                  val tree = q"""$mods def $methodName[..$tpes](...$argsRes): $returnType =  {
                  assert(${c.parse(input)}, ${"Input constraint not kept: " + input})
                  val ${c.parse(out)} = {..$body}
                  ${c.parse(out)}
                }"""
                  println(showCode(tree))
                  tree
                case _ => c.abort(c.enclosingPosition, "No match")
              }
            }

          }
          case _ => c.abort(c.enclosingPosition, "Annotation @refine can be used only with methods")
        }
      }
      c.Expr[Any](result)
    }
  }




}

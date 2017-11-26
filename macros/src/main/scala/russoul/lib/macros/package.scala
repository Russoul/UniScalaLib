package russoul.lib


import machinist.{DefaultOperatorNames, DefaultOps, Ops}

import scala.collection.immutable
import scala.language.higherKinds
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
    def apply(xs: Any*) = macro at_impl
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
      val natTpeTree = c.parse(s"""shapeless.Nat._${xs.size.toString}""")
      val tree = q"russoul.lib.common.math.algebra.Vec.apply[$tpeTree,$natTpeTree](..$xs)"
      c.Expr(tree)
    }
  }


}

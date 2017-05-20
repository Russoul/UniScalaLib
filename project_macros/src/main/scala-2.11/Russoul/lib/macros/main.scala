package Russoul.lib.macros


//import scala.meta.{Lit, Stat, _}
import scala.meta._
//import scala.reflect.macros.blackbox._
/**
  * Created by russoul on 17.05.17.
  */
class VecMeta extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case dff @ Defn.Class(mods, typename, tps, ctor, temp) =>

        println(ctor.structure)

        dff
      case _ => q"()"
    }
  }
}

/*object Macros{
  def impl(c: Context)(): c.Expr[Real[Float]] = {
    import c.universe._

    //println(showRaw(s))
    val s = c.Expr[String](Select(c.prefix.tree, TermName("s")))

    println(showRaw(s))
    s match {
      case Expr(Select(Apply(Select(Ident(_), TermName(_)), List(Apply(Select(Select(Ident(scala), _), TermName("apply")), List(Literal(Constant(x:String)))))), TermName(_))) =>  {
        val float = x.toFloat
        c.Expr(q"Real($float)")
      }
      case _ => c.abort(c.enclosingPosition, "not a compile-time constant")
    }


  }
}*/

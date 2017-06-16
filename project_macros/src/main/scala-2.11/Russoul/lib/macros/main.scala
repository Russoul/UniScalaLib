package Russoul.lib.macros



import scala.reflect.macros.blackbox._
import scala.language.experimental.macros

object Macros {
  def doubleAsField[A : c.WeakTypeTag](c:Context)(ev : c.Expr[Field[A]]):c.Expr[A]= {
    import c.universe._
    c.Expr[A](c.prefix.tree match {
      case Apply((_, List(Literal(Constant(0.0))))) => q"$ev.zero"
      case Apply((_, List(Literal(Constant(1.0))))) => q"$ev.one"
      case Apply((_, List(n))) => q"$ev.fromDouble($n)"
    })
  }
}


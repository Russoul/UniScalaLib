package Russoul.lib.macros


//import scala.meta.{Lit, Stat, _}

//import scala.meta._





/*object Macros {
  def implFloat(c: Context)(str: c.Expr[String]): c.Expr[Float] = {
    import c.universe._

    println(showRaw(str))

    str match{
      case Expr(Literal(Constant(x:String))) => {
          println(showRaw(c.Expr(q"${x.toFloat}")) )
          c.Expr(q"${x.toFloat}")
        }
      case _ =>
        c.abort(c.enclosingPosition, "not a compile-time constant")
    }

  }*/

  /*def impl(c: Context)(): c.Expr[Any] = {
    import c.universe._

    val exp = c.parse("implicitly[FieldLike[Double]].fromDouble(tt.toDouble)")
    val ev = c.eval[Double](c.Expr(exp))
    println(exp)
    println(ev)

    /*str match{
      case Expr(Literal(Constant(x:String))) =>
        c.Expr(q"(implicitly[FieldLike[${tag.tpe}]].fromDouble(s.s()))")
      case _ =>
        c.abort(c.enclosingPosition, "not a compile-time constant")
    }*/

    c.Expr(exp)

  }*/


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

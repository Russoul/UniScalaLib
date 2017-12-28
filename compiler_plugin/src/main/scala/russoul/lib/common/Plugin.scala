package russoul.lib.common


import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._
class CompilerPlugin(override val global: Global)
  extends Plugin {
  override val name = "compiler-plugin"
  override val description = "Compiler plugin"
  override val components =
    List(new CompilerPluginComponent(global))
}
object Patterns{
  val pattern1 = """Ref\["\s*(.*)\s*[^\|]\|[^\|]\s*(.*)"\.type\]""".r.unanchored
  val pattern2 = """Ref\["\s*(.*)"\.type\]""".r.unanchored
}
class CompilerPluginComponent(val global: Global)
  extends PluginComponent with TypingTransformers {

  import global._

  override val phaseName = "compiler-plugin-phase"
  override val runsAfter = List("parser")

  override def newPhase(prev: Phase) =
    new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        unit.body = new MyTypingTransformer(unit).transform(unit.body)
      }
    }

  class MyTypingTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit) {



    override def transform(tree: Tree) = {
      tree match {
        case dd: DefDef if dd.vparamss.length >= 2 =>
          val ps = dd.vparamss.last.toString()
          ps match {
            case Patterns.pattern1(_, _) | Patterns.pattern2(_) =>

              //println(showRaw(dd.mods.annotations(0)))

              val annot = Apply(Select(New(Select(Select(Select(Ident(TermName("russoul")), TermName("lib")), TermName("macros")), TypeName("ref"))), termNames.CONSTRUCTOR), List())
              val newAnnots = dd.mods.annotations :+ annot
              val mods = Modifiers(dd.mods.flags, dd.mods.privateWithin, newAnnots)
              val tree = q"""${mods} def ${dd.name}[..${dd.tparams}](...${dd.vparamss}): ${dd.tpt} =  {
                  ${dd.rhs}
              }"""

              //println(tree)

              tree



            case _ => super.transform(tree)
          }





        case _ => super.transform(tree)
      }
    }
  }
  def newTransformer(unit: CompilationUnit) =
    new MyTypingTransformer(unit)
}



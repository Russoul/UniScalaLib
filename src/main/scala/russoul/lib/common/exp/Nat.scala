package russoul.lib.common.exp

object Nat{

  trait Nat{
    type N <: Nat
  }
  sealed class Succ[P <: Nat] extends Nat{
    type N = Succ[P]
  }

  sealed class _0 extends Nat{
    override type N = _0
  }


  val _0: _0 = new _0

  trait ToInt[N <: Nat] extends Serializable {
    def apply() : Int
  }

  object ToInt {
    def apply[N <: Nat](implicit toInt: ToInt[N]): ToInt[N] = toInt

    implicit val toInt0 = new ToInt[_0] {
      def apply() = 0
    }
    implicit def toIntSucc[N <: Nat](implicit toIntN : ToInt[N]) = new ToInt[Succ[N]] {
      def apply() = toIntN()+1
    }
  }

}

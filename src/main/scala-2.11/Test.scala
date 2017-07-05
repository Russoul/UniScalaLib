import shapeless._
import singleton.ops._

/**
  * Created by russoul on 03.07.2017.
  */
object Test extends App{



  def testSO(): Unit ={


    def demo[L <: XInt](implicit p : L*L + L) : p.Out {} = p.value
    val b : 30 = demo[5]

  }

  testSO()
}

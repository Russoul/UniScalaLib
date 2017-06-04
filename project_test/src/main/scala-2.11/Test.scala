



object Test extends App {

  class Mat(i:Int, j:Int)
  class Mat4 extends Mat(4,4)

  trait Gram
  object Gram{
    def gen(mat: Mat) : Mat with Gram = {
      new Mat(0,0) with Gram
    }
  }

  trait MatWorks[T <: Mat]{
    def func(mat: T) : T
  }

  object MatWorks4 extends MatWorks[Mat4]{
    override def func(mat: Mat4): Mat4 = {
      mat
    }
  }

  val mat = new Mat(1,1)
  val mat4 = new Mat4


  MatWorks4.func(mat4)

  def funcOnMatGram(mat: Mat with Gram): Unit ={

  }

  funcOnMatGram(mat)

}
